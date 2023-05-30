module Main (main) where

import Vega.Prelude

import Vega.Pretty (ANSI (..), pretty)

import Vega.Error qualified as Error
import Vega.Lexer qualified as Lexer
import Vega.Parser qualified as Parser
import Vega.ToLua qualified as ToLua
import Vega.Types qualified as Types

main :: IO ()
main = do
    let ?style = ANSI

    contents <- decodeUtf8 <$> readFileBS "test.vega"

    tokens <- case Lexer.run "test.vega" contents of
        Right tokens -> pure tokens
        Left error -> do
            putTextLn ("Lexical error: " <> pretty (Error.LexError error))
            exitFailure

    program <- case Parser.runParserM (Parser.parse tokens) of
        Right program -> pure program
        Left error -> do
            putTextLn ("Parse error: " <> pretty (Error.ParseError error))
            exitFailure

    typed <-
        Types.runTypeM (Types.typecheck program) >>= \case
            Right program -> pure program
            Left error -> do
                putTextLn (pretty (Error.TypeError error))
                exitFailure

    let luaCode = ToLua.compile typed

    writeFileText "test.lua" luaCode
    pure ()