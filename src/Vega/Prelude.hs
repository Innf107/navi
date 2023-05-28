module Vega.Prelude (
    module Export,
    HsType,
    mapAccumRM,
) where

import Control.Monad.Except as Export
import Relude as Export hiding (Type, force)
import Relude qualified
import Relude.Extra as Export

type HsType = Relude.Type

mapAccumRM
    :: (Traversable t, Monad m)
    => (s -> a -> m (s, b))
    -> s
    -> t a
    -> m (s, t b)
mapAccumRM f state elements =
    fmap swap $ flip runStateT state $ forM elements \x -> do
        state <- get
        (newState, result) <- lift (f state x)
        put newState
        pure result
