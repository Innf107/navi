module Vega.Types (
    TypeError (..),
    typecheck,
    runTypeM,
) where

import Vega.Prelude

import Vega.Syntax

import Vega.Cached (cached, force)

import GHC.Show qualified as Show

runTypeM :: TypeM a -> IO (Either TypeError a)
runTypeM (MkTypeM typeM) = runExceptT typeM

typeError :: TypeError -> TypeM a
typeError error = MkTypeM (throwError error)

bind :: Name -> TypeValue -> TypeEnv -> TypeEnv
bind name typeValue env =
    env
        { varTypes = insert name typeValue env.varTypes
        }

infer :: TypeEnv -> Expr Parsed -> TypeM (Expr Typed, TypeValue)
infer env = \case
    Var name ->
        case lookup name env.varTypes of
            Nothing -> error "Vega.Types.infer: Variable not found during type check"
            Just ty -> pure (Var name, ty)
    App funExpr argExpr -> do
        (funExpr, funType) <- infer env funExpr
        case funType of
            Pi name argType body piEnv -> do
                undefined
                argExpr <- check env argType argExpr
                let updatedEnv = bind name argType env
                resultType <- eval updatedEnv body
                pure (App funExpr argExpr, resultType)
            funType -> typeError (ApplicationOfNonPi funType)
    Lambda _ _ -> typeError (UnableToInferLambda)
    Sequence statements -> undefined
    PiLit name typeExpr body -> do
        typeExpr <- check env Type typeExpr
        typeValue <- eval env typeExpr

        let updatedEnv = case name of
                -- If there is no name in the source syntax, we don't need to bind anything.
                -- This might change if we ever switch to DeBruijn indices.
                Nothing -> env
                Just name -> bind name typeValue env

        undefined
    -- Yes Type : Type. I honestly couldn't care less about logical consistency
    TypeLit -> pure (TypeLit, Type)

check :: TypeEnv -> TypeValue -> Expr Parsed -> TypeM (Expr Typed)
check env expectedType = \case
    _ -> undefined

checkDecl :: TypeEnv -> Decl Parsed -> TypeM (TypeEnv, Decl Typed)
checkDecl env = \case
    DeclVar name typeExpr body -> do
        typeExpr <- check env Type typeExpr
        typeValue <- eval env typeExpr
        body <- check env typeValue body

        let updatedEnv = bind name typeValue env

        pure (updatedEnv, DeclVar name typeExpr body)

eval :: TypeEnv -> Expr Typed -> TypeM Value
eval env = \case
    Var name ->
        case lookup name env.varValues of
            Nothing -> undefined
            Just delayed -> force delayed
    App funExpr argExpr -> undefined
    _ -> undefined

typecheck :: Program Parsed -> TypeM (Program Typed)
typecheck Program{declarations} = do
    let initialEnv =
            TypeEnv
                { varTypes = mempty
                , varValues = mempty
                }

    (_newEnv, typedDecls) <- mapAccumRM (\env decl -> checkDecl env decl) initialEnv declarations
    pure $
        Program
            { declarations = typedDecls
            }
