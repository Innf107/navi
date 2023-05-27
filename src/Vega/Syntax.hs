module Vega.Syntax (
    Pass (..),
    Name,
    Expr (..),
    TypeExpr,
    Decl (..),
    Program (..),
    Value (..),
    TypeValue,
) where

import Vega.Prelude

type Name = Text

data Pass = Parsed | Typed

data Expr (pass :: Pass)
    = Var Name
    | App (Expr pass) (Expr pass)
    | Lambda Name (Expr pass)
    | Let Name (Maybe (TypeExpr pass)) (Expr pass)
    | PiLit Name (TypeExpr pass) (Expr pass)
    | TypeLit

type TypeExpr = Expr

data Decl pass
    = DeclVar Name (TypeExpr pass) (Expr pass)

data Program pass = Program
    { declarations :: [Decl pass]
    }

data Value
    = Pi Name Value (TypeExpr Typed)
    | Type
    | StuckVar Name
    | StuckApp Value Value

type TypeValue = Value
