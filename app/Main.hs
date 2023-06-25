module Main (main) where

import Vega.Prelude

import Vega.Config qualified as Config
import Vega.Pretty (ANSI (..), pretty)

import Data.Text.IO (hPutStrLn)
import Vega.Error qualified as Error
import Vega.Lexer qualified as Lexer
import Vega.Parser qualified as Parser
import Vega.ToLua qualified as ToLua
import Vega.Types qualified as Types

data ArgPresence = Partial | Completed
type family Entry presence a where
    Entry Partial a = Maybe a
    Entry Completed a = a

data ArgsF p = Args
    { file :: Entry p FilePath
    }
type Args = ArgsF Completed

parseArgs :: [String] -> IO Args
parseArgs = go Args{file = Nothing}
  where
    go :: ArgsF Partial -> [String] -> IO Args
    go options [] = validate options
    go options ("--print-debruijn" : args) = do
        Config.setPrintDeBruijn True
        go options args
    go _ (flag : _)
        | "-" `isPrefixOf` flag = failUsage $ "Invalid flag '" <> toText flag <> "'"
    go options@Args{file = Nothing} (arg : args) =
        go options{file = Just arg} args
    go Args{file = Just name} (arg : _) =
        failUsage $ "Too many arguments for argument FILE: '" <> toText name <> "' and '" <> toText arg <> "'"

    validate :: ArgsF Partial -> IO (ArgsF Completed)
    validate Args{file = Nothing} =
        failUsage "Missing FILE"
    validate Args{file = Just file} =
        pure $ Args{file}

failUsage :: Text -> IO a
failUsage message = do
    hPutStrLn stderr $
        message
            <> "\n\n"
            <> "Usage: vega [OPTIONS] <FILE>\n"
            <> "\n"
            <> "Options"
            <> "    --print-debruijn    Display DeBruijn levels and indices of variables in error messages"
    exitFailure

main :: IO ()
main = do
    let ?style = ANSI

    Args{file} <- parseArgs =<< getArgs

    contents <- decodeUtf8 <$> readFileBS file

    tokens <- case Lexer.run file contents of
        Right tokens -> pure tokens
        Left error -> do
            putTextLn ("Lexical error: " <> pretty (Error.LexError error))
            exitFailure

    program <- case Parser.runParserM (Parser.parse tokens) of
        Right program -> pure program
        Left error -> do
            putTextLn (pretty (Error.ParseError error))
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