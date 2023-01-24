module Graphics.HEGL.ExprID (
    ExprID,
    genID
) where

import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

type ExprID = Int

{-# NOINLINE unsafeCounter #-}
unsafeCounter :: IORef Int
unsafeCounter = unsafePerformIO $ newIORef 0

{-# NOINLINE genID #-}
genID :: () -> ExprID
genID () = unsafePerformIO $ do
    id <- readIORef unsafeCounter
    writeIORef unsafeCounter (id + 1)
    return id
