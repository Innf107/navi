{
{-# LANGUAGE NoStrictData #-}
module Vega.Parser (parse, ParseError(..), ParserM, runParserM) where

import Prelude hiding (lex)

import Relude (Text)

import Vega.Syntax
import Vega.Lexer (Token(..))

import Control.Monad.Except (ExceptT, throwError, runExceptT)

}

%name parse program
%tokentype { Token }
%error { parseError }

%monad { ParserM }

%token ident            { IDENT $$ }
%token 'λ'              { LAMBDA }
%token 'let'            { LET }
%token 'Type'           { TYPE }
%token ':'              { COLON }
%token '->'             { ARROW }
%token '='              { EQUALS }
%token '('              { LPAREN }
%token ')'              { RPAREN }
%token '{'              { LBRACE }
%token '}'              { RBRACE }
%token ';'              { SEMI }
%token eof              { EOF }

%%

program :: { Program Parsed }
program : declarations { Program { declarations = $1 } }

declarations :: { [Decl Parsed] }
declarations :                       { [] }
             | decl                  { [$1] }
             | decl ';' declarations { $1 : $3 }

decl :: { Decl Parsed }
decl : ident ':' expr ';' ident '=' expr { if $1 /= $5 then undefined else DeclVar $1 $3 $7 }

expr :: { Expr Parsed }
expr : expr expr_leaf { App $1 $2 }
     | expr_leaf      { $1 }

expr_leaf :: { Expr Parsed }
expr_leaf : ident                               { Var $1 }
          | 'λ' ident '->' expr                 { Lambda $2 $4 }
          | '{' statements '}'                  { Sequence $2 }
          | '(' ident ':' expr ')' '->' expr    { PiLit (Just $2) $4 $7 }
          | expr_leaf '->' expr                 { PiLit Nothing $1 $3 }
          | 'Type'                              { TypeLit }
          | '(' expr ')'                        { $2 }

statement :: { Statement Parsed }
statement : 'let' ident '=' expr            { Let $2 Nothing $4 }
          | 'let' ident ':' expr '=' expr   { Let $2 (Just $4) $6 }
          | expr { RunExpr $1 }

statements :: { [Statement Parsed] }
statements :                          { [] }
           | statement                { [$1] }
           | statement ';' statements { $1 : $3 }


{
data ParseError 
    = ParseError [Token]
    | UnexpectedEOF
    deriving (Show)

newtype ParserM a = MkParserM (Either ParseError a) deriving (Functor, Applicative, Monad)

runParserM :: ParserM a -> Either ParseError a
runParserM (MkParserM parserM) = parserM
    
parseError :: [Token] -> ParserM a
parseError [] = MkParserM (throwError UnexpectedEOF)
parseError tokens = MkParserM (throwError (ParseError tokens))
}
