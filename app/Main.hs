module Main (main) where

import Vega.Prelude

import Vega.Pretty (ANSI (..), pretty)

import Vega.Error qualified as Error
import Vega.Lexer qualified as Lexer
import Vega.Parser qualified as Parser
import Vega.Types qualified as Types

main :: IO ()
main = do
    let ?style = ANSI

    contents <- decodeUtf8 <$> readFileBS "test.vega"

    tokens <- case Lexer.run contents of
        Right tokens -> pure tokens
        Left error -> fail (toString $ "Lexical error: " <> pretty (Error.LexError error))

    putStrLn ("Tokens: " <> show tokens)

    program <- case Parser.runParserM (Parser.parse tokens) of
        Right program -> pure program
        Left error -> fail (toString $ "Parse error: " <> pretty (Error.ParseError error))

    putStrLn ("Program: " <> show program)
    typed <-
        Types.runTypeM (Types.typecheck program) >>= \case
            Right program -> pure program
            Left error -> fail (toString $ "Type error: " <> pretty (Error.TypeError error))

    putStrLn ("Typed: " <> show typed)
