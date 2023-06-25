{-# LANGUAGE OverloadedRecordDot #-}

module Vega.Types (
    TypeError (..),
    typecheck,
    runTypeM,
) where

import Vega.Prelude

import Vega.Syntax

import Vega.Delay (Delay, delay, delayValue, force)

import Vega.DeBruijn (Level, addVariable, emptyVariables, getVariable, levelToIndex, nextLevel, variableEntries)

import Data.Foldable (foldrM)
import Data.Map qualified as Map
import Data.Unique (hashUnique, newUnique)
import Vega.Pretty qualified as Pretty

runTypeM :: TypeM a -> IO (Either TypeError a)
runTypeM (MkTypeM typeM) = runExceptT typeM

typeError :: TypeError -> TypeM a
typeError error = MkTypeM (throwError error)


bind :: Maybe Name -> TypeValue -> Delay TypeM Value -> TypeEnv -> TypeEnv
bind name typeValue value env = do
    let (evalEnv, level) = bindEval value env.evalEnv
    env
        { variables =
            case name of
                Just name -> insert name (typeValue, level) env.variables
                Nothing -> env.variables
        , evalEnv
        }

bindEval :: Delay TypeM Value -> EvalEnv -> (EvalEnv, Level)
bindEval value evalEnv = do
    let (varValues, level) = addVariable value evalEnv.varValues
    (evalEnv{varValues}, level)

bindStuck :: Maybe Name -> TypeValue -> TypeEnv -> TypeM TypeEnv
bindStuck mname ty env = do
    varValue <- delayValue (StuckVar (fromMaybe "α" mname) (nextLevel env.evalEnv.varValues))
    pure $ bind mname ty varValue env

bindStuckEval :: Name -> EvalEnv -> TypeM EvalEnv
bindStuckEval name env = do
    varValue <- delayValue (StuckVar name (nextLevel env.varValues))
    pure $ fst $ bindEval varValue env

data CheckOrInfer a where
    Check :: TypeValue -> CheckOrInfer ()
    Infer :: CheckOrInfer TypeValue

infer :: TypeEnv -> Expr Parsed -> TypeM (Expr Typed, TypeValue)
infer env = \case
    Var loc () name ->
        case lookup name env.variables of
            Nothing -> error ("Vega.Types.infer: Variable not found during type check: " <> name)
            Just (ty, level) -> do
                let index = levelToIndex level env.evalEnv.varValues
                pure (Var loc index name, ty)
    App loc funExpr argExpr -> do
        (funExpr, funType) <- infer env funExpr
        case funType of
            PiClosure name argType body piEnv -> do
                argExpr <- check env argType argExpr

                argValue <- delay (eval env.evalEnv argExpr)
                let (updatedEnv, _level) = bindEval argValue piEnv

                traceShowM =<< force argValue
                resultType <- eval updatedEnv body
                pure (App loc funExpr argExpr, resultType)
            funType -> typeError (ApplicationOfNonPi loc funType)
    Lambda loc _ _ -> typeError (UnableToInferLambda loc)
    Sequence loc statements -> do
        (statements, ty) <- checkStatements Infer loc env statements
        pure (Sequence loc statements, ty)
    Pi loc name typeExpr body -> do
        typeExpr <- check env Type typeExpr

        traceM (let ?style = Pretty.ANSI in toString $ Pretty.pretty typeExpr)
        typeValue <- eval env.evalEnv typeExpr

        updatedEnv <- bindStuck name typeValue env

        body <- check updatedEnv Type body
        pure (Pi loc name typeExpr body, Type)
    -- Yes Type : Type. Fight me
    TypeLit loc -> pure (TypeLit loc, Type)

check :: TypeEnv -> TypeValue -> Expr Parsed -> TypeM (Expr Typed)
check env expectedType expr = do
    let deferToInference = do
            (expr, ty) <- infer env expr
            subsumes (getLoc expr) env.evalEnv ty expectedType
            pure expr
    case expr of
        Var _ () _ -> deferToInference
        App _ _ _ -> deferToInference
        Lambda loc name body -> do
            case expectedType of
                -- TODO: What if 'expectedType' is a Stuck{Var,App}?
                PiClosure piName domain codomain piEnv -> do
                    bodyEnv <- bindStuck (Just name) domain env

                    codomain <- evalClosureAbstractMaybe piName codomain piEnv 

                    body <- check bodyEnv codomain body
                    pure (Lambda loc name body)
                _ -> typeError (DefiningLambdaAsNonPi loc expectedType)
        Sequence _ _ -> undefined
        Pi{} -> deferToInference
        TypeLit _ -> deferToInference

checkStatements :: CheckOrInfer result -> Loc -> TypeEnv -> [Statement Parsed] -> TypeM ([Statement Typed], result)
checkStatements Infer _seqLoc env [RunExpr expr] = do
    (expr, ty) <- infer env expr
    pure ([RunExpr expr], ty)
checkStatements Infer _seqLoc _env [] = pure ([], undefined) -- TODO: Return () here
checkStatements (Check ty) seqLoc env [] = do
    subsumes seqLoc env.evalEnv ty undefined
    pure ([], ()) -- TODO: ()
checkStatements checkOrInfer seqLoc env (RunExpr expr : statements) = do
    expr <- check env undefined expr -- TODO: ()
    (statements, result) <- checkStatements checkOrInfer seqLoc env statements
    pure (RunExpr expr : statements, result)
checkStatements checkOrInfer seqLoc env (Let loc name maybeTypeExpr body : statements) = do
    -- Regular let bindings are non-recursive, so we just use the ambient environment
    (body, varType, maybeTypeExpr) <- case maybeTypeExpr of
        Nothing -> do
            (body, varType) <- infer env body
            pure (body, varType, Nothing)
        Just typeExpr -> do
            typeExpr <- check env Type typeExpr
            typeValue <- eval env.evalEnv typeExpr

            body <- check env typeValue body
            pure (body, typeValue, Just typeExpr)

    bodyValue <- delay (eval env.evalEnv body)

    let updatedEnv = bind (Just name) varType bodyValue env
    (statements, result) <- checkStatements checkOrInfer seqLoc updatedEnv statements
    pure (Let loc name maybeTypeExpr body : statements, result)

checkDecl :: TypeEnv -> Decl Parsed -> TypeM (TypeEnv, Decl Typed)
checkDecl env = \case
    DeclVar loc name typeExpr body -> do
        typeExpr <- check env Type typeExpr
        typeValue <- eval env.evalEnv typeExpr

        traceM $ toString $ "declVar: " <> name <> " : " <> (let ?style = Pretty.ANSI in Pretty.pretty typeValue)

        body <- check env typeValue body


        bodyValue <- delay (eval env.evalEnv body)
        let updatedEnv = bind (Just name) typeValue bodyValue env

        pure (updatedEnv, DeclVar loc name typeExpr body)
    DeclFunction loc functionName typeExpr params body -> do
        -- Function definitions *can* be recursive
        typeExpr <- check env Type typeExpr
        typeValue <- eval env.evalEnv typeExpr

        (paramsWithTypes, bodyType) <- splitFunctionType loc typeValue params

        envWithParams <-
            foldrM
                ( \(name, ty) env ->
                    bindStuck (Just name) ty env
                )
                env
                paramsWithTypes

        -- TODO: I hope not including the real value here is fine?
        bodyEnv <- bindStuck (Just functionName) typeValue envWithParams

        body <- check bodyEnv bodyType body

        -- TODO: Construct the actual value with closures here rather than a stuck value
        -- TODO: Couldn't we use the closure inside the body as well?
        updatedEnv <- bindStuck (Just functionName) typeValue env

        pure (updatedEnv, DeclFunction loc functionName typeExpr params body)

splitFunctionType :: Loc -> TypeValue -> [Name] -> TypeM ([(Name, TypeValue)], TypeValue)
splitFunctionType _loc ty [] = pure ([], ty)
splitFunctionType loc (PiClosure mname domain codomain piEnv) (param : params) = do
    codomain <- evalClosureAbstractMaybe mname codomain piEnv

    (rest, result) <- splitFunctionType loc codomain params
    pure ((param, domain) : rest, result)
splitFunctionType loc _ty params = typeError (MoreArgumentsThanInType loc (length params))

evalClosureAbstractMaybe :: Maybe Name -> Expr Typed -> EvalEnv -> TypeM Value
evalClosureAbstractMaybe mname expr env = do
    env <- bindStuckEval (fromMaybe "α" mname) env
    eval env expr

eval :: EvalEnv -> Expr Typed -> TypeM Value
eval env = \case
    expr@(Var loc index name) -> do
        traceM ("eval: " <> toString (let ?style = Pretty.ANSI in Pretty.pretty expr))
        case getVariable index env.varValues of
            Nothing -> do
                errorEvalEnv env (show loc <> ": Undefined variable " <> show name <> " at out of range index " <> show index)
            Just delayed -> do
                val <- force delayed
                traceM ("evalV: " <> toString (let ?style = Pretty.ANSI in Pretty.pretty val))
                pure val
    App _ funExpr argExpr -> do
        funValue <- eval env funExpr

        argValue <- delay $ eval env argExpr
        case funValue of
            StuckVar{} -> StuckApp funValue <$> force argValue
            StuckApp{} -> StuckApp funValue <$> force argValue
            LambdaClosure _name body env -> do
                eval (fst $ bindEval argValue env) body
            PiClosure{} -> error "Tried evaluating unsound application of non-function value"
            Type -> error "Tried evaluating unsound application of non-function value"
    (TypeLit _) -> pure Type
    Pi _ maybeName domain codomain -> do
        domain <- eval env domain
        pure (PiClosure maybeName domain codomain env)
    Lambda _ name body ->
        pure $ LambdaClosure name body env
    Sequence _ statements -> do
        undefined

typecheck :: Program Parsed -> TypeM (Program Typed)
typecheck Program{declarations} = do
    let initialEnv =
            TypeEnv
                { variables = mempty
                , evalEnv =
                    EvalEnv
                        { varValues = emptyVariables
                        }
                }

    (_newEnv, typedDecls) <- mapAccumRM (\env decl -> checkDecl env decl) initialEnv declarations
    pure $
        Program
            { declarations = typedDecls
            }

-- | Assert that one type should be a subtype of another. (TODO: Will this ever actually do subtyping?)
subsumes :: Loc -> EvalEnv -> TypeValue -> TypeValue -> TypeM ()
subsumes loc env originalTy1 originalTy2 = do
    traceM $ toString $ let ?style = Pretty.ANSI in "subsumes: " <> Pretty.pretty originalTy1 <> " <= " <> Pretty.pretty originalTy2
    go originalTy1 originalTy2
  where
    unificationContext = (originalTy1, originalTy2)

    go Type val = case val of
        Type -> pure ()
        _ -> typeError (UnableToUnify loc Type val unificationContext)
    go (StuckVar name1 level1) val = case val of
        (StuckVar _ level2) | level1 == level2 -> pure ()
        _ -> typeError (UnableToUnify loc (StuckVar name1 level1) val unificationContext)
    go (StuckApp fun1 arg1) val = case val of
        StuckApp fun2 arg2 -> do
            go fun1 fun2
            go arg1 arg2
        _ -> typeError (UnableToUnify loc Type val unificationContext)
    go val1@(PiClosure name1 domain1 codomain1 piEnv1) val = case val of
        PiClosure name2 domain2 codomain2 piEnv2 -> do
            -- This is contravariant so the order is swapped
            go domain2 domain1

            -- TODO: How the fuck does this work?! The closure environments are different,
            -- so this level might mean something very different in them doesn't it?
            let level = nextLevel env.varValues

            -- The names only exist for debugging purposes, so there is no issue if they are different.
            -- The important part is that they have the same level
            skolem1 <- delayValue (StuckVar (fromMaybe "α" $ name1) level)
            skolem2 <- delayValue (StuckVar (fromMaybe "α" $ name2) level)

            let (updatedEnv1, _) = bindEval skolem1 piEnv1
            let (updatedEnv2, _) = bindEval skolem2 piEnv2

            codomainValue1 <- eval updatedEnv1 codomain1
            codomainValue2 <- eval updatedEnv2 codomain2

            go codomainValue1 codomainValue2
        _ -> typeError (UnableToUnify loc val1 val unificationContext)
    go val1@(LambdaClosure name1 body1 env1) val = case val of
        LambdaClosure _name2 body2 env2 -> do
            skolem <- delayValue (StuckVar name1 (nextLevel env.varValues))

            bodyValue1 <- eval (fst $ bindEval skolem env1) body1
            bodyValue2 <- eval (fst $ bindEval skolem env2) body2

            go bodyValue1 bodyValue2
        _ -> typeError (UnableToUnify loc val1 val unificationContext)

displayTypeEnv :: (?style :: style, Pretty.TextStyle style) => TypeEnv -> TypeM (Pretty.Doc style)
displayTypeEnv TypeEnv{variables, evalEnv} = do
    evalEnvDoc <- displayEvalEnv evalEnv
    pure $
        Pretty.emphasis "------TYPES------\n"
            <> Pretty.intercalate
                (Pretty.literal "\n")
                ( map
                    (\(name, (ty, _)) -> Pretty.identifier name <> Pretty.literal " : " <> Pretty.pretty ty)
                    (sortOn (\(_, (_, level)) -> level) (Map.toList variables))
                )
            <> Pretty.literal "\n"
            <> evalEnvDoc

displayEvalEnv :: (?style :: style, Pretty.TextStyle style) => EvalEnv -> TypeM (Pretty.Doc style)
displayEvalEnv EvalEnv{varValues} = do
    valueDocs <- traverse (\value -> Pretty.pretty <$> force value) (variableEntries varValues)
    pure $
        Pretty.emphasis "------VALUES------\n"
            <> Pretty.intercalate
                (Pretty.literal "\n")
                valueDocs
            <> Pretty.emphasis
                "\n-----------------\n"

errorEnv :: HasCallStack => TypeEnv -> Text -> TypeM a
errorEnv env message = do
    let ?style = Pretty.Plain
    envDoc <- displayTypeEnv env
    MkTypeM $ writeFileText "_vegaEnv" envDoc
    error message

errorEvalEnv :: HasCallStack => EvalEnv -> Text -> TypeM a
errorEvalEnv env message = do
    let ?style = Pretty.Plain
    envDoc <- displayEvalEnv env
    MkTypeM $ writeFileText "_vegaEnv" envDoc
    error message
