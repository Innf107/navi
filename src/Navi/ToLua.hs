module Navi.ToLua (compile) where

import Navi.Prelude

import Navi.Syntax

import Data.Text qualified as Text

type LuaProgram = Text

compile :: Program Typed -> LuaProgram
compile Program{declarations} =
    Text.intercalate "\n\n" (map compileDecl declarations)

compileDecl :: Decl Typed -> Text
compileDecl (DeclVar _ name _ty body) =
    "local " <> name <> " = " <> compileExpr body
compileDecl (DeclFunction _ _ _ [] _) = error "Navi.ToLua.compileDecl: DeclFunction without parameters"
compileDecl (DeclFunction loc name _typeExpr (param : params) body) =
    "local function "
        <> name
        <> "("
        <> param
        <> ") "
        <> compileExpr (foldr (Lambda loc) body params)
        <> " end"

compileExpr :: Expr Typed -> Text
compileExpr (Var _ _ name) = name
compileExpr (App _ funExpr argExpr) = compileExpr funExpr <> "(" <> compileExpr argExpr <> ")"
compileExpr (Lambda _ name body) = "(function (" <> name <> ") return " <> compileExpr body <> " end)"
compileExpr Pi{} = "nil" -- 'Pi's should not stick around at runtime
compileExpr (TypeLit _) = "nil" -- Neither should Type literals
compileExpr (Sequence _ statements) = "(function ()\n" <> compileStatements statements <> "\nend)()"

compileStatements :: [Statement Typed] -> Text
compileStatements (Let _ name _ty body : rest) =
    "local "
        <> name
        <> " = "
        <> compileExpr body
        <> "\n"
        <> compileStatements rest
compileStatements [RunExpr expr] = "return " <> compileExpr expr
compileStatements (RunExpr expr : rest) =
    -- Not every Navi expression is a valid lua statement so we bind it
    -- to an unused variable to be sure
    "local _ = "
        <> compileExpr expr
        <> "\n"
        <> compileStatements rest
compileStatements [] = ""
