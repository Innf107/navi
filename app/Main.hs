module Main (main) where

import Vega.Prelude

import Vega.Lexer qualified as Lexer
import Vega.Parser qualified as Parser

main :: IO ()
main = do
    contents <- decodeUtf8 <$> readFileBS "test.vega"

    tokens <- case Lexer.run contents of
            Right tokens -> pure tokens
            Left error -> fail ("Lexical error: " <> show error)
        
    putStrLn ("Tokens: " <> show tokens)

    program <- case Parser.runParserM (Parser.parse tokens) of
            Right program -> pure program
            Left error -> fail ("Parse error: " <> show error)

    putStrLn ("Program: " <> show program)


