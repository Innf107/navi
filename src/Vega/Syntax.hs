module Vega.Syntax (
    Pass (..),
    Name,
    Expr (..),
    Statement (..),
    TypeExpr,
    Decl (..),
    Program (..),
    Value (..),
    TypeValue,
    TypeEnv (..),
    TypeM (..),
    TypeError (..),
) where

import Vega.Prelude

import Vega.Cached (Cached)
import Vega.Pretty qualified as Pretty

import Data.Text qualified as Text

type Name = Text

data Pass = Parsed | Typed

data Expr (pass :: Pass)
    = Var Name
    | App (Expr pass) (Expr pass)
    | Lambda Name (Expr pass)
    | Sequence [Statement pass]
    | PiLit (Maybe Name) (TypeExpr pass) (Expr pass)
    | TypeLit
    deriving (Show)

data Statement (pass :: Pass)
    = Let Name (Maybe (TypeExpr pass)) (Expr pass)
    | RunExpr (Expr pass)
    deriving (Show)

type TypeExpr = Expr

data Decl pass
    = DeclVar Name (TypeExpr pass) (Expr pass)
    deriving (Show)

data Program pass = Program
    { declarations :: [Decl pass]
    }
    deriving (Show)

data Value
    = Pi Name Value (TypeExpr Typed) TypeEnv
    | Type
    | StuckVar Name
    | StuckApp Value Value

type TypeValue = Value

data TypeEnv = TypeEnv
    { varTypes :: Map Name TypeValue
    , varValues :: Map Name (Cached TypeM TypeValue)
    }

-- This needs to be defined here since TypeEnv depends on the type of the cached monad and Value
-- captures the environment :/
newtype TypeM a
    = MkTypeM (ExceptT TypeError IO a)
    deriving (Functor, Applicative, Monad, MonadIO)

data TypeError
    = ApplicationOfNonPi TypeValue
    | UnableToInferLambda

instance Pretty.Pretty Value where
    pretty :: (?style :: style, Pretty.TextStyle style) => Value -> Pretty.Doc style
    pretty = \case
        Pi name ty body _env ->
            Pretty.paren "("
                <> Pretty.identifier name
                <> Pretty.literal " "
                <> Pretty.operator ":"
                <> Pretty.literal " "
                <> Pretty.pretty ty
                <> Pretty.paren ")"
                <> Pretty.literal " "
                <> Pretty.operator "->"
                <> Pretty.literal " "
                <> Pretty.pretty body
        Type -> Pretty.keyword "Type"
        StuckVar name -> Pretty.identifier name
        StuckApp funValue argValue ->
            Pretty.paren "(" <> Pretty.pretty funValue <> Pretty.literal " " <> Pretty.pretty argValue <> Pretty.paren ")"

instance Pretty.Pretty (Expr pass) where
    pretty = \case
        Var name -> Pretty.identifier name
        App funExpr argExpr ->
            Pretty.paren "("
                <> Pretty.pretty funExpr
                <> Pretty.literal " "
                <> Pretty.pretty argExpr
                <> Pretty.paren ")"
        Lambda name body ->
            Pretty.operator "\\"
                <> Pretty.identifier name
                <> Pretty.literal " "
                <> Pretty.operator "->"
                <> Pretty.literal " "
                <> Pretty.pretty body
        Sequence statements ->
            Pretty.paren "{"
                <> Pretty.intercalate (Pretty.operator ";") (map Pretty.pretty statements)
                <> Pretty.paren "}"
        PiLit Nothing ty body ->
            Pretty.paren "("
                <> Pretty.pretty ty
                <> Pretty.paren ")"
                <> Pretty.literal " "
                <> Pretty.operator "->"
                <> Pretty.literal " "
                <> Pretty.pretty body
        PiLit (Just name) ty body ->
            Pretty.paren "("
                <> Pretty.identifier name
                <> Pretty.literal " "
                <> Pretty.operator ":"
                <> Pretty.literal " "
                <> Pretty.pretty ty
                <> Pretty.paren ")"
                <> Pretty.literal " "
                <> Pretty.operator "->"
                <> Pretty.literal " "
                <> Pretty.pretty body
        TypeLit -> Pretty.keyword "Type"

instance Pretty.Pretty (Statement pass)
