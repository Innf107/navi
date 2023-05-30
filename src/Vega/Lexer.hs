module Vega.Lexer (
    run,
    Token (..),
    TokenClass (..),
    LexError (..),
) where

import Vega.Prelude

import Vega.Loc (Loc (..))

import Data.Char qualified as Char
import Data.Text qualified as Text

data TokenClass
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

data Token = Token TokenClass Loc

data LexError
    = UnexpectedChar Char
    | UnexpectedEOF

data LexState = LexState
    { underlying :: Text
    , fileName :: String
    , startColumn :: Int
    , startLine :: Int
    , endColumn :: Int
    , endLine :: Int
    }

newtype LexM a = MkLexM (ExceptT LexError (State LexState) a) deriving (Functor, Applicative, Monad)

emit :: TokenClass -> LexM Token
emit tokenClass = MkLexM $ do
    LexState{fileName, startLine, startColumn, endLine, endColumn} <- get
    pure (Token tokenClass (Loc{fileName, startLine, startColumn, endLine, endColumn}))

peekChar :: LexM (Maybe Char)
peekChar = do
    LexState{underlying} <- MkLexM (lift get)
    pure $ fst <$> Text.uncons underlying

advance :: LexM ()
advance = MkLexM $ lift $ modify \state -> case Text.uncons state.underlying of
    Nothing -> state
    Just (char, rest) ->
        case char of
            '\n' ->
                state
                    { underlying = rest
                    , Vega.Lexer.endColumn = 1
                    , Vega.Lexer.endLine = state.endLine + 1
                    }
            _ ->
                state
                    { underlying = rest
                    , Vega.Lexer.endColumn = state.endColumn + 1
                    }

advanceWhitespace :: LexM ()
advanceWhitespace = MkLexM $ modify \state ->
    state
        { Vega.Lexer.startColumn = state.endColumn
        , Vega.Lexer.startLine = state.endLine
        }

lexError :: LexError -> LexM a
lexError = MkLexM . throwError

lex :: LexM Token
lex = do
    advanceWhitespace
    peekChar >>= \case
        Nothing -> emit EOF
        Just '\\' -> advance >> emit LAMBDA
        Just ':' -> advance >> emit COLON
        Just '=' -> advance >> emit EQUALS
        Just '(' -> advance >> emit LPAREN
        Just ')' -> advance >> emit RPAREN
        Just '{' -> advance >> emit LBRACE
        Just '}' -> advance >> emit RBRACE
        Just ';' -> advance >> emit SEMI
        Just '-' -> do
            advance
            peekChar >>= \case
                Just '>' -> advance >> emit ARROW
                Just '-' -> advance >> lexLineComment
                Just char -> lexError (UnexpectedChar char)
                Nothing -> lexError UnexpectedEOF
        Just char
            | Char.isSpace char -> advance >> lex
            | Char.isAlpha char -> advance >> lexIdent [char]
            | otherwise -> lexError (UnexpectedChar char)

lexLineComment :: LexM Token
lexLineComment =
    peekChar >>= \case
        Nothing -> emit EOF
        Just '\n' -> advance >> lex
        Just _ -> advance >> lexLineComment

lexIdent :: [Char] -> LexM Token
lexIdent chars =
    peekChar >>= \case
        Nothing -> emit builtIdent
        Just char
            | Char.isAlphaNum char -> advance >> lexIdent (char : chars)
            | otherwise -> emit builtIdent
  where
    builtIdent =
        case reverse chars of
            "let" -> LET
            "Type" -> TYPE
            ident -> IDENT (toText ident)

runLexM :: FilePath -> Text -> LexM a -> Either LexError a
runLexM fileName text (MkLexM lexM) = do
    let initialState =
            LexState
                { underlying = text
                , fileName
                , startLine = 1
                , startColumn = 1
                , endLine = 1
                , endColumn = 1
                }
    evalState (runExceptT lexM) initialState

run :: FilePath -> Text -> Either LexError [Token]
run fileName source =
    runLexM fileName source $ fix \recurse ->
        lex >>= \case
            Token EOF _ -> pure []
            token -> (token :) <$> recurse
