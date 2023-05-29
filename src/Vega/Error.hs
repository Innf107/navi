module Vega.Error (Error (..)) where

import Vega.Prelude

import Vega.Pretty qualified as Pretty

import Vega.Lexer qualified as Lexer
import Vega.Parser qualified as Parser
import Vega.Types qualified as Types

data Error
    = LexError Lexer.LexError
    | ParseError Parser.ParseError
    | TypeError Types.TypeError

instance Pretty.Pretty Error where
    pretty (LexError err) = case err of
        Lexer.UnexpectedChar char ->
            Pretty.literal "Unexpected character: "
                <> Pretty.identifier (show char)
        Lexer.UnexpectedEOF ->
            Pretty.literal "Unexpected end of file"
    pretty (ParseError err) = case err of
        Parser.ParseError tokens ->
            Pretty.literal "Parse error\n"
                <> Pretty.literal
                    ( "  next tokens: "
                        <> show (take 10 tokens)
                    )
        Parser.UnexpectedEOF -> Pretty.literal "Unexpected end of file"
    pretty (TypeError err) = case err of
        Types.UnableToUnify actual expected ->
            Pretty.literal "Unable to unify\n"
            <> Pretty.literal "       expected type " <> Pretty.pretty expected <> Pretty.literal "\n"
            <> Pretty.literal "    with actual type " <> Pretty.pretty actual
        Types.ApplicationOfNonPi value ->
            Pretty.literal "Application of non-pi value: " <> Pretty.pretty value
        Types.UnableToInferLambda ->
            Pretty.literal "Unable to infer type of lambda expression.\n"
                <> Pretty.literal "  Please provide an explicit type signature"
        Types.DefiningLambdaAsNonPi ty ->
            Pretty.literal "Lambda expression is expected to have non-function type " <> Pretty.pretty ty
        Types.MoreArgumentsThanInType number ->
            Pretty.literal "Function definition is defined with too many arguments.\n"
            <> Pretty.literal "It has " <> Pretty.number number <> Pretty.literal " more than its type suggests"
