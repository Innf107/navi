module Navi.DeBruijn (
    Level,
    Index,
    Variables,
    emptyVariables,
    addVariable,
    getVariable,
    variableEntries,
    levelToIndex,
    nextLevel,
) where

import Navi.Prelude

import Data.IntMap qualified as IntMap

newtype Level = MkLevel Int
    deriving newtype (Show, Eq, Ord)

newtype Index = MkIndex Int deriving newtype (Show, Eq, Ord)

newtype Variables a = MkVariables (IntMap a)

emptyVariables :: Variables a
emptyVariables = MkVariables mempty

addVariable :: a -> Variables a -> (Variables a, Level)
addVariable a (MkVariables map) = (MkVariables $ insert (size map) a map, MkLevel (size map))

getVariable :: Index -> Variables a -> Maybe a
getVariable (MkIndex index) (MkVariables map) =
    lookup (size map - index - 1) map

levelToIndex :: Level -> Variables a -> Index
levelToIndex (MkLevel level) (MkVariables map) = MkIndex (size map - level - 1)

nextLevel :: Variables a -> Level
nextLevel (MkVariables map) = MkLevel (size map)

variableEntries :: Variables a -> [a]
variableEntries (MkVariables variables) =
    map snd $ sortOn fst $ IntMap.toList variables
