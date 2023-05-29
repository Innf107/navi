module Vega.Pretty (
    Pretty (..),
    TextStyle (..),
    intercalate,
    ANSI (ANSI),
) where

import Vega.Prelude hiding (intercalate)

class Pretty a where
    pretty :: (?style :: style, TextStyle style) => a -> Doc style

class (Monoid (Doc a)) => TextStyle a where
    literal :: (?style :: a) => Text -> Doc a

    identifier :: (?style :: a) => Text -> Doc a
    keyword :: (?style :: a) => Text -> Doc a
    operator :: (?style :: a) => Text -> Doc a
    paren :: (?style :: a) => Text -> Doc a
    error :: (?style :: a) => Text -> Doc a
    emphasis :: (?style :: a) => Text -> Doc a
    number :: (?style :: a, Num number, Show number) => number -> Doc a

    type Doc a

intercalate :: (Monoid a) => a -> [a] -> a
intercalate _ [] = mempty
intercalate _ [x] = x
intercalate sep (x : xs) = x <> sep <> intercalate sep xs

data ANSI = ANSI

instance TextStyle ANSI where
    literal text = text

    identifier text = "\x1b[1m\STX" <> text <> "\x1b[0m\STX"
    keyword text = "\x1b[96m\STX" <> text <> "\x1b[0m\STX"
    operator text = "\x1b[94m\STX" <> text <> "\x1b[0m\STX"
    paren = operator
    error text = "\x1b[31m\STX" <> text <> "\x1b[0m\STX"
    emphasis text = "\x1b[1m\STX" <> text <> "\x1b[0m\STX"
    number num = "\x1b[1m\x1b[93m\STX" <> show num <> "\x1b[0m\STX"

    type Doc ANSI = Text
