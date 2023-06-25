module Navi.Delay (
    Delay,
    delay,
    delayValue,
    force,
) where

import Navi.Prelude

{- | 'Delay m a' represents a lazy effectful computation in a Monad 'm'.
    Repeatedly forcing a delayed value will only compute the result, and
    therefore only perform the contained effects once.

    This is necessary, since Haskell's regular laziness mechanisms only apply in
    pure code (modulo unsafePerformIO).
-}
newtype Delay m a = MkDelay (IORef (Either (m a) a))

{- | Delay a monadic computation. Any effects performed by the argument will only take effect
    the first time the result is 'force'd.
-}
delay :: MonadIO m => m a -> m (Delay m a)
delay action = MkDelay <$> newIORef (Left action)

{- | Construct an immediately forced delayed value. This can be passed to functions that expect
    a delayed value without the overhead of delaying the computation
-}
delayValue :: MonadIO m => a -> m (Delay m a)
delayValue value = MkDelay <$> newIORef (Right value)

{- | Force a delayed computation in Monad 'm'. Forcing will only re-evaluate the computation
    once and subsequent applications of 'force' will return the same value.

    This is not thread safe! Only use this if you know that either
        a) this is only ever going to run on one thread
        b) the computation is morally pure and two racing computations will
           produce the same result
-}
force :: MonadIO m => Delay m a -> m a
force (MkDelay ref) =
    readIORef ref >>= \case
        Left action -> do
            result <- action
            writeIORef ref (Right result)
            pure result
        Right value -> pure value
