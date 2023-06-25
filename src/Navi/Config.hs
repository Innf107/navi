module Navi.Config (setPrintDeBruijn, getPrintDeBruijn) where

import System.IO.Unsafe (unsafePerformIO)
import Navi.Prelude

printDeBruijnRef :: IORef Bool
printDeBruijnRef = unsafePerformIO $ newIORef False
{-# NOINLINE printDeBruijnRef #-}

setPrintDeBruijn :: Bool -> IO ()
setPrintDeBruijn value = writeIORef printDeBruijnRef value

getPrintDeBruijn :: () -> Bool
getPrintDeBruijn () = unsafePerformIO $ readIORef printDeBruijnRef
{-# NOINLINE getPrintDeBruijn #-}
