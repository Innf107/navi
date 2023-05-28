module Vega.Cached (
    Cached,
    cached,
    cachedValue,
    force,
) where

import Vega.Prelude

{- | 'Cached m a' represents a lazy effectful computation in a Monad 'm'.
    Repeatedly forcing a cached value will only compute the result, and
    therefore only perform the contained effects once.

    This is necessary, since Haskell's regular laziness mechanisms only apply in
    pure code (modulo unsafePerformIO).
-}
newtype Cached m a = MkCached (IORef (Either (m a) a))

{- | Cached a monadic computation. Any effects performed by the argument will only take effect
    the first time the result is 'force'd.
-}
cached :: MonadIO m => m a -> m (Cached m a)
cached action = MkCached <$> newIORef (Left action)

{- | Construct an immediately forced cached value. This can be passed to functions that expect
    a cached value without the overhead of delaying the computation
-}
cachedValue :: MonadIO m => a -> m (Cached m a)
cachedValue value = MkCached <$> newIORef (Right value)

{- | Force a delayed computation in Monad 'm'. Forcing will only re-evaluate the computation
    once and subsequent applications of 'force' will return the same value.

    This is not thread safe! Only use this if you know that either
        a) this is only ever going to run on one thread
        b) the computation is morally pure and two racing computations will
           produce the same result
-}
force :: MonadIO m => Cached m a -> m a
force (MkCached ref) =
    readIORef ref >>= \case
        Left action -> do
            result <- action
            writeIORef ref (Right result)
            pure result
        Right value -> pure value
