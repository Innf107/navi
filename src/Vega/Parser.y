{
{-# LANGUAGE NoStrictData #-}
module Vega.Parser (parse, ParseError(..), ParserM, runParserM) where

import Prelude hiding (lex)

import Relude (Text)

import Vega.Syntax
import Vega.Lexer (Token(..), TokenClass(..))
import Vega.Loc (Loc(..), merge)

import Control.Monad.Except (ExceptT, throwError, runExceptT)

}

%name parse program
%tokentype { Token }
%error { parseError }

%monad { ParserM }

%token identLoc         { (\case { Token (IDENT name) loc -> Just (name, loc); _ -> Nothing } -> Just $$) }
%token 'λ'              { Token LAMBDA $$ }
%token 'let'            { Token LET $$ }
%token 'Type'           { Token TYPE $$ }
%token ':'              { Token COLON $$ }
%token '->'             { Token ARROW $$ }
%token '='              { Token EQUALS $$ }
%token '('              { Token LPAREN $$ }
%token ')'              { Token RPAREN $$ }
%token '{'              { Token LBRACE $$ }
%token '}'              { Token RBRACE $$ }
%token ';'              { Token SEMI $$ }
%token eof              { Token EOF $$ }

%%

ident :: { Name }
ident : identLoc { fst $1 }

program :: { Program Parsed }
program : declarations { Program { declarations = $1 } }

declarations :: { [Decl Parsed] }
declarations :                       { [] }
             | decl                  { [$1] }
             | decl ';' declarations { $1 : $3 }

decl :: { Decl Parsed }
decl : identLoc ':' expr ';' ident '=' expr                  
          {% if fst $1 /= $5 then throwParseError (MismatchedDeclName (snd $1) (fst $1) $5) else pure $ DeclVar (merge (snd $1) $7) (fst $1) $3 $7 }
     | identLoc ':' expr ';' ident ident ident_list '=' expr 
          {% if fst $1 /= $5 then throwParseError (MismatchedDeclName (snd $1) (fst $1) $5) else pure $ DeclFunction (merge (snd $1) $9) (fst $1) $3 ($6 : $7) $9 }


expr :: { Expr Parsed }
expr : expr expr_leaf { App (merge $1 $2) $1 $2 }
     | expr_leaf      { $1 }

expr_leaf :: { Expr Parsed }
expr_leaf : identLoc                            { Var (snd $1) () (fst $1) }
          | 'λ' ident '->' expr                 { Lambda (merge $1 $4) $2 $4 }
          | '{' statements '}'                  { Sequence (merge $1 $3) $2 }
          | '(' ident ':' expr ')' '->' expr    { Pi (merge $1 $7) (Just $2) $4 $7 }
          | expr_leaf '->' expr                 { Pi (merge $1 $3) Nothing $1 $3 }
          | 'Type'                              { TypeLit $1 }
          | '(' expr ')'                        { $2 }

statement :: { Statement Parsed }
statement : 'let' ident '=' expr            { Let (merge $1 $4) $2 Nothing $4 }
          | 'let' ident ':' expr '=' expr   { Let (merge $1 $6) $2 (Just $4) $6 }
          | expr { RunExpr $1 }

statements :: { [Statement Parsed] }
statements :                          { [] }
           | statement                { [$1] }
           | statement ';' statements { $1 : $3 }

ident_list :: { [Name] }
ident_list :                  { [] }
           | ident ident_list { $1 : $2 }


{
data ParseError 
    = ParseError [Token]
    | UnexpectedEOF
    | MismatchedDeclName Loc Text Text

newtype ParserM a = MkParserM (Either ParseError a) deriving (Functor, Applicative, Monad)

throwParseError :: ParseError -> ParserM a
throwParseError err = MkParserM (Left err)

runParserM :: ParserM a -> Either ParseError a
runParserM (MkParserM parserM) = parserM
    
parseError :: [Token] -> ParserM a
parseError [] = MkParserM (throwError UnexpectedEOF)
parseError tokens = MkParserM (throwError (ParseError tokens))
}
