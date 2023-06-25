module Navi.Loc (
    Loc (..),
    HasLoc (..),
    merge,
) where

import Navi.Prelude

import GHC.Show qualified as S

data Loc = Loc
    { fileName :: FilePath
    , startLine :: Int
    , startColumn :: Int
    , endLine :: Int
    , endColumn :: Int
    }

merge :: (HasLoc withLoc1, HasLoc withLoc2) => withLoc1 -> withLoc2 -> Loc
merge withLoc1 withLoc2 = do
    let loc1 = getLoc withLoc1
    let loc2 = getLoc withLoc2
    Loc
        { fileName = loc1.fileName
        , startLine = loc1.startLine
        , startColumn = loc1.startColumn
        , endLine = loc2.endLine
        , endColumn = loc2.endColumn
        }

instance S.Show Loc where
    show Loc{fileName, startLine, startColumn, endLine, endColumn} =
        fileName <> ":" <> show startLine <> ":" <> show startColumn <> " - " <> show endLine <> ":" <> show endColumn

class HasLoc a where
    getLoc :: a -> Loc

instance HasLoc Loc where
    getLoc = id
