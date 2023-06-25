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
    EvalEnv (..),
    TypeM (..),
    TypeError (..),
    Loc,
    getLoc,
) where

import Vega.Prelude

import Vega.Config qualified as Config
import Vega.DeBruijn (Index, Level, Variables)
import Vega.Delay (Delay)
import Vega.Loc (HasLoc (..), Loc)
import Vega.Pretty qualified as Pretty

import GHC.Show qualified as Show

type Name = Text

data Pass = Parsed | Typed

data Expr (pass :: Pass)
    = Var Loc (XVar pass) Name
    | App Loc (Expr pass) (Expr pass)
    | Lambda Loc Name (Expr pass)
    | Sequence Loc [Statement pass]
    | Pi Loc (Maybe Name) (TypeExpr pass) (Expr pass)
    | TypeLit Loc

type family XVar (pass :: Pass) where
    XVar Parsed = ()
    XVar Typed = Index

data Statement (pass :: Pass)
    = Let Loc Name (Maybe (TypeExpr pass)) (Expr pass)
    | RunExpr (Expr pass)

type TypeExpr = Expr

data Decl pass
    = DeclVar Loc Name (TypeExpr pass) (Expr pass)
    | DeclFunction Loc Name (TypeExpr pass) [Name] (Expr pass)

data Program pass = Program
    { declarations :: [Decl pass]
    }

data Value
    = PiClosure (Maybe Name) Value (TypeExpr Typed) EvalEnv
    | LambdaClosure Name (TypeExpr Typed) EvalEnv
    | Type
    | StuckVar Name Level
    | StuckApp Value Value

type TypeValue = Value

data TypeEnv = TypeEnv
    { variables :: Map Name (TypeValue, Level)
    , evalEnv :: EvalEnv
    }

newtype EvalEnv = EvalEnv
    { varValues :: Variables (Delay TypeM Value)
    }

-- This needs to be defined here since TypeEnv depends on the type of the cached monad and Value
-- captures the environment :/
newtype TypeM a
    = MkTypeM (ExceptT TypeError IO a)
    deriving (Functor, Applicative, Monad, MonadIO)

type UnificationContext = (TypeValue, TypeValue)

data TypeError
    = UnableToUnify Loc TypeValue TypeValue UnificationContext
    | ApplicationOfNonPi Loc TypeValue
    | UnableToInferLambda Loc
    | DefiningLambdaAsNonPi Loc TypeValue
    | MoreArgumentsThanInType Loc Int

instance Pretty.Pretty Value where
    pretty :: (?style :: style, Pretty.TextStyle style) => Value -> Pretty.Doc style
    pretty = \case
        PiClosure Nothing ty body _env ->
            Pretty.paren "("
                <> Pretty.pretty ty
                <> Pretty.paren ")"
                <> Pretty.literal " "
                <> Pretty.operator "->"
                <> Pretty.literal " "
                <> Pretty.pretty body
        PiClosure (Just name) ty body _env ->
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
        LambdaClosure name body _env ->
            Pretty.paren "("
                <> Pretty.operator "\\"
                <> Pretty.identifier name
                <> Pretty.literal " "
                <> Pretty.operator "->"
                <> Pretty.literal " "
                <> Pretty.pretty body
                <> Pretty.paren ")"
        Type -> Pretty.keyword "Type"
        StuckVar name level ->
            if Config.getPrintDeBruijn ()
                then Pretty.identifier name <> Pretty.note ("$" <> show level)
                else Pretty.identifier name
        StuckApp funValue argValue ->
            Pretty.paren "(" <> Pretty.pretty funValue <> Pretty.literal " " <> Pretty.pretty argValue <> Pretty.paren ")"

instance Show.Show Value where
    show = let ?style = Pretty.Plain in toString . Pretty.pretty

class PrettyXVar a where
    prettyIndex
        :: (?style :: style, Pretty.TextStyle style)
        => Pretty.Doc style
        -> a
        -> Pretty.Doc style

instance PrettyXVar () where prettyIndex doc () = doc
instance PrettyXVar Index where
    prettyIndex doc index =
        if Config.getPrintDeBruijn ()
            then doc <> Pretty.note ("@" <> show index)
            else doc

instance PrettyXVar (XVar pass) => Pretty.Pretty (Expr pass) where
    pretty = \case
        Var _ index name -> prettyIndex (Pretty.identifier name) index
        App _ funExpr argExpr ->
            Pretty.paren "("
                <> Pretty.pretty funExpr
                <> Pretty.literal " "
                <> Pretty.pretty argExpr
                <> Pretty.paren ")"
        Lambda _ name body ->
            Pretty.operator "\\"
                <> Pretty.identifier name
                <> Pretty.literal " "
                <> Pretty.operator "->"
                <> Pretty.literal " "
                <> Pretty.pretty body
        Sequence _ statements ->
            Pretty.paren "{"
                <> Pretty.intercalate (Pretty.operator ";") (map Pretty.pretty statements)
                <> Pretty.paren "}"
        Pi _ Nothing ty body ->
            Pretty.paren "("
                <> Pretty.pretty ty
                <> Pretty.paren ")"
                <> Pretty.literal " "
                <> Pretty.operator "->"
                <> Pretty.literal " "
                <> Pretty.pretty body
        Pi _ (Just name) ty body ->
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
        TypeLit _ -> Pretty.keyword "Type"

instance (PrettyXVar (XVar pass)) => Pretty.Pretty (Statement pass) where
    pretty = \case
        Let _ name Nothing expr ->
            Pretty.keyword "let"
                <> Pretty.literal " "
                <> Pretty.identifier name
                <> Pretty.literal " "
                <> Pretty.operator "="
                <> Pretty.literal " "
                <> Pretty.pretty expr
        Let _ name (Just ty) expr ->
            Pretty.keyword "let"
                <> Pretty.literal " "
                <> Pretty.identifier name
                <> Pretty.literal " "
                <> Pretty.operator ":"
                <> Pretty.literal " "
                <> Pretty.pretty ty
                <> Pretty.literal " "
                <> Pretty.operator "="
                <> Pretty.literal " "
                <> Pretty.pretty expr
        RunExpr expr -> Pretty.pretty expr

instance HasLoc (Expr pass) where
    getLoc = \case
        Var loc _ _ -> loc
        App loc _ _ -> loc
        Lambda loc _ _ -> loc
        Sequence loc _ -> loc
        Pi loc _ _ _ -> loc
        TypeLit loc -> loc
