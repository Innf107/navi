module Vega.Types (
    TypeError (..),
    typecheck,
    runTypeM,
) where

import Vega.Prelude

import Vega.Syntax

import Vega.Cached (Cached, cached, cachedValue, force)

import Data.Unique (hashUnique, newUnique)

runTypeM :: TypeM a -> IO (Either TypeError a)
runTypeM (MkTypeM typeM) = runExceptT typeM

typeError :: TypeError -> TypeM a
typeError error = MkTypeM (throwError error)

freshVar :: Name -> TypeM Name
-- TODO: This should be very... different
freshVar name = MkTypeM do
    unique <- liftIO newUnique
    pure (name <> show (hashUnique unique))

bind :: Name -> TypeValue -> Cached TypeM Value -> TypeEnv -> TypeEnv
bind name typeValue value env =
    env
        { varTypes = insert name typeValue env.varTypes
        , evalEnv =
            env.evalEnv
                { varValues = insert name value env.evalEnv.varValues
                }
        }

bindEval :: Name -> Cached TypeM Value -> EvalEnv -> EvalEnv
bindEval name value evalEnv =
    evalEnv{varValues = insert name value evalEnv.varValues}

data CheckOrInfer a where
    Check :: TypeValue -> CheckOrInfer ()
    Infer :: CheckOrInfer TypeValue

infer :: TypeEnv -> Expr Parsed -> TypeM (Expr Typed, TypeValue)
infer env = \case
    Var name ->
        case lookup name env.varTypes of
            Nothing -> error ("Vega.Types.infer: Variable not found during type check: " <> name)
            Just ty -> pure (Var name, ty)
    App funExpr argExpr -> do
        (funExpr, funType) <- infer env funExpr
        case funType of
            PiClosure name argType body piEnv -> do
                argExpr <- check env argType argExpr

                argValue <- cached (eval env.evalEnv argExpr)
                let updatedEnv = case name of
                        Just name -> bindEval name argValue piEnv
                        Nothing -> env.evalEnv
                resultType <- eval updatedEnv body
                pure (App funExpr argExpr, resultType)
            funType -> typeError (ApplicationOfNonPi funType)
    Lambda _ _ -> typeError (UnableToInferLambda)
    Sequence statements -> do
        (statements, ty) <- checkStatements Infer env statements
        pure (Sequence statements, ty)
    Pi name typeExpr body -> do
        typeExpr <- check env Type typeExpr
        typeValue <- eval env.evalEnv typeExpr

        updatedEnv <- case name of
            -- If there is no name in the source syntax, we don't need to bind anything.
            -- This might change if we ever switch to DeBruijn indices.
            Nothing -> pure env
            Just name -> do
                varValue <- cachedValue (StuckVar name)
                pure $ bind name typeValue varValue env

        body <- check updatedEnv Type body
        pure (Pi name typeExpr body, Type)
    -- Yes Type : Type. I honestly couldn't care less about logical consistency
    TypeLit -> pure (TypeLit, Type)

check :: TypeEnv -> TypeValue -> Expr Parsed -> TypeM (Expr Typed)
check env expectedType expr = do
    let deferToInference = do
            (expr, ty) <- infer env expr
            subsumes ty expectedType
            pure expr
    case expr of
        Var _ -> deferToInference
        App _ _ -> deferToInference
        Lambda name body -> do
            case expectedType of
                PiClosure piName domain codomain piEnv -> do
                    varValue <- cachedValue (StuckVar name)
                    let updatedEnv = bind name domain varValue env

                    codomain <- case piName of
                        Just piName -> do
                            piVarValue <- cachedValue (StuckVar piName)
                            eval (bindEval piName piVarValue piEnv) codomain
                        Nothing -> eval piEnv codomain

                    body <- check updatedEnv codomain body
                    pure (Lambda name body)
                _ -> typeError (DefiningLambdaAsNonPi expectedType)
        Sequence _ -> undefined
        Pi{} -> deferToInference
        TypeLit -> deferToInference

checkStatements :: CheckOrInfer result -> TypeEnv -> [Statement Parsed] -> TypeM ([Statement Typed], result)
checkStatements Infer env [RunExpr expr] = do
    (expr, ty) <- infer env expr
    pure ([RunExpr expr], ty)
checkStatements Infer _env [] = pure ([], undefined) -- TODO: Return () here
checkStatements (Check ty) _env [] = do
    subsumes ty undefined
    pure ([], ()) -- TODO: ()
checkStatements checkOrInfer env (RunExpr expr : statements) = do
    expr <- check env undefined expr -- TODO: ()
    (statements, result) <- checkStatements checkOrInfer env statements
    pure (RunExpr expr : statements, result)
checkStatements checkOrInfer env (Let name maybeTypeExpr body : statements) = do
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

    bodyValue <- cached (eval env.evalEnv body)

    let updatedEnv = bind name varType bodyValue env
    (statements, result) <- checkStatements checkOrInfer updatedEnv statements
    pure (Let name maybeTypeExpr body : statements, result)

checkDecl :: TypeEnv -> Decl Parsed -> TypeM (TypeEnv, Decl Typed)
checkDecl env = \case
    DeclVar name typeExpr body -> do
        typeExpr <- check env Type typeExpr
        typeValue <- eval env.evalEnv typeExpr
        body <- check env typeValue body

        bodyValue <- cached (eval env.evalEnv body)
        let updatedEnv = bind name typeValue bodyValue env

        pure (updatedEnv, DeclVar name typeExpr body)
    DeclFunction functionName typeExpr params body -> do
        -- Function definitions *can* be recursive
        typeExpr <- check env Type typeExpr
        typeValue <- eval env.evalEnv typeExpr

        (paramsWithTypes, bodyType) <- splitFunctionType typeValue params

        paramsWithTypesAndValues <- forM paramsWithTypes \(name, ty) -> do 
            value <- cachedValue (StuckVar name)
            pure (name, ty, value)

        let envWithParams = foldr (\(name, ty, value) env -> bind name ty value env) env paramsWithTypesAndValues

        -- TODO: I hope not including the real value here is fine?
        functionValue <- cachedValue (StuckVar functionName)
        let bodyEnv = bind functionName typeValue functionValue envWithParams

        body <- check bodyEnv bodyType body

        let updatedEnv = bind functionName typeValue functionValue env

        pure (updatedEnv, DeclFunction functionName typeExpr params body)


splitFunctionType :: TypeValue -> [Name] -> TypeM ([(Name, TypeValue)], TypeValue)
splitFunctionType ty [] = pure ([], ty)
splitFunctionType (PiClosure mname domain codomain piEnv) (param : params) = do
    codomain <- evalClosureAbstractMaybe mname codomain piEnv

    (rest, result) <- splitFunctionType codomain params
    pure ((param, domain) : rest, result)
splitFunctionType _ty params = typeError (MoreArgumentsThanInType (length params))

evalClosureAbstractMaybe :: Maybe Name -> Expr Typed -> EvalEnv -> TypeM Value
evalClosureAbstractMaybe mname expr env = do
    env <- case mname of
        Nothing -> pure env
        Just name -> do
            value <- cachedValue (StuckVar name)
            pure $ bindEval name value env
    eval env expr

eval :: EvalEnv -> Expr Typed -> TypeM Value
eval env = \case
    Var name ->
        case lookup name env.varValues of
            Nothing -> pure $ StuckVar name
            Just delayed -> force delayed
    App funExpr argExpr -> do
        funValue <- eval env funExpr

        argValue <- cached $ eval env argExpr
        case funValue of
            StuckVar{} -> StuckApp funValue <$> force argValue
            StuckApp{} -> StuckApp funValue <$> force argValue
            LambdaClosure name body env -> do
                eval (bindEval name argValue env) body
            PiClosure{} -> error "Tried evaluating unsound application of non-function value"
            Type -> error "Tried evaluating unsound application of non-function value"
    TypeLit -> pure Type
    Pi maybeName domain codomain -> do
        domain <- eval env domain
        pure (PiClosure maybeName domain codomain env)
    Lambda name body ->
        pure $ LambdaClosure name body env
    Sequence statements -> do
        undefined

typecheck :: Program Parsed -> TypeM (Program Typed)
typecheck Program{declarations} = do
    let initialEnv =
            TypeEnv
                { varTypes = mempty
                , evalEnv =
                    EvalEnv
                        { varValues = mempty
                        }
                }

    (_newEnv, typedDecls) <- mapAccumRM (\env decl -> checkDecl env decl) initialEnv declarations
    pure $
        Program
            { declarations = typedDecls
            }

-- | Assert that one type should be a subtype of another.
subsumes :: TypeValue -> TypeValue -> TypeM ()
subsumes Type val = case val of
    Type -> pure ()
    _ -> typeError (UnableToUnify Type val)
subsumes (StuckVar name1) val = case val of
    -- TODO: Fuck, this is completely unsound. I either need a renamer or DeBruijn indices
    (StuckVar name2) | name1 == name2 -> pure ()
    _ -> typeError (UnableToUnify (StuckVar name1) val)
subsumes (StuckApp fun1 arg1) val = case val of
    StuckApp fun2 arg2 -> do
        subsumes fun1 fun2
        subsumes arg1 arg2
    _ -> typeError (UnableToUnify Type val)
subsumes val1@(PiClosure name1 domain1 codomain1 env1) val = case val of
    PiClosure name2 domain2 codomain2 env2 -> do
        -- This is contravariant so the order is swapped
        subsumes domain2 domain1
        -- TODO: Ideally we should use different names for debugging, but the same indices or something
        -- (although we need to makes sure to display that to the user correctly)
        skolem <- cachedValue . StuckVar =<< freshVar (fromMaybe "a" $ name1 <|> name2)

        let updatedEnv1 = case name1 of
                Nothing -> env1
                Just name1 -> bindEval name1 skolem env1
        let updatedEnv2 = case name2 of
                Nothing -> env2
                Just name2 -> bindEval name2 skolem env2

        codomainValue1 <- eval updatedEnv1 codomain1
        codomainValue2 <- eval updatedEnv2 codomain2

        subsumes codomainValue1 codomainValue2
    _ -> typeError (UnableToUnify val1 val)
subsumes val1@(LambdaClosure name1 body1 env1) val = case val of
    LambdaClosure name2 body2 env2 -> do
        -- TODO: We should not arbitrarily pick one name here (see PiClosure above)
        skolem <- cachedValue (StuckVar name1)

        bodyValue1 <- eval (bindEval name1 skolem env1) body1
        bodyValue2 <- eval (bindEval name2 skolem env2) body2

        subsumes bodyValue1 bodyValue2
    _ -> typeError (UnableToUnify val1 val)