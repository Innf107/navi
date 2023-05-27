module Vega.Lexer (
    run,
    Token (..),
    LexError (..),
) where

import Vega.Prelude

import Data.Char qualified as Char
import Data.Text qualified as Text

data Token
    = IDENT Text
    | LET
    | TYPE
    | LAMBDA
    | COLON
    | ARROW
    | EQUALS
    | LPAREN
    | RPAREN
    | LBRACE
    | RBRACE
    | SEMI
    | EOF
    deriving (Show)

data LexError
    = UnexpectedChar Char
    | UnexpectedEOF
    deriving (Show)

data LexState = LexState
    { underlying :: Text
    }

newtype LexM a = MkLexM (ExceptT LexError (State LexState) a) deriving (Functor, Applicative, Monad)

peekChar :: LexM (Maybe Char)
peekChar = do
    LexState{underlying} <- MkLexM (lift get)
    pure $ fst <$> Text.uncons underlying

advance :: LexM ()
advance = MkLexM $ lift $ modify \state -> case Text.uncons state.underlying of
    Nothing -> state
    Just (_, rest) ->
        state{underlying = rest}

lexError :: LexError -> LexM a
lexError = MkLexM . throwError

lex :: LexM Token
lex =
    peekChar >>= \case
        Nothing -> pure EOF
        Just '\\' -> advance >> pure LAMBDA
        Just ':' -> advance >> pure COLON
        Just '=' -> advance >> pure EQUALS
        Just '(' -> advance >> pure LPAREN
        Just ')' -> advance >> pure RPAREN
        Just '{' -> advance >> pure LBRACE
        Just '}' -> advance >> pure RBRACE
        Just ';' -> advance >> pure SEMI
        Just '-' -> do
            advance
            peekChar >>= \case
                Just '>' -> advance >> pure ARROW
                Just char -> lexError (UnexpectedChar char)
                Nothing -> lexError UnexpectedEOF
        Just char
            | Char.isSpace char -> advance >> lex
            | Char.isAlpha char -> advance >> lexIdent [char]
            | otherwise -> lexError (UnexpectedChar char)

lexIdent :: [Char] -> LexM Token
lexIdent chars =
    peekChar >>= \case
        Nothing -> pure builtIdent
        Just char
            | Char.isAlpha char -> advance >> lexIdent (char : chars)
            | otherwise -> pure builtIdent
  where
    builtIdent =
        case reverse chars of
            "let" -> LET
            "Type" -> TYPE
            ident -> IDENT (toText ident)

runLexM :: Text -> LexM a -> Either LexError a
runLexM text (MkLexM lexM) = do
    let initialState = LexState{underlying = text}
    evalState (runExceptT lexM) initialState

run :: Text -> Either LexError [Token]
run source =
    runLexM source $ fix \recurse -> lex >>= \case
        EOF -> pure []
        token -> (token :) <$> recurse
