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
    TypeEnvM (..),
) where

import Vega.Prelude

import Vega.Cached (Cached)

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
    = Pi Name Value (TypeExpr Typed)
    | Type
    | StuckVar Name
    | StuckApp Value Value

type TypeValue = Value

data TypeEnvM m = TypeEnv
    { varTypes :: Map Name TypeValue
    , varValues :: Map Name (Cached m Value)
    }
