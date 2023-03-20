module Graphics.HEGL.ExprID (
    ExprID,
    genID,
    idLabel,
    HasExprID(..)
) where

import Prelude hiding (id)
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

type ExprID = Int

{-# NOINLINE unsafeCounter #-}
unsafeCounter :: IORef Int
unsafeCounter = unsafePerformIO $ newIORef 0

-- TODO: this approach isn't ideal (not because of the unsafePerformIO);
-- ideally replace it with a deterministic hash-based one
-- All modules using this function should be compiled with -fno-cse!!
{-# NOINLINE genID #-}
genID :: () -> ExprID
genID () = unsafePerformIO $ do
    id <- readIORef unsafeCounter
    writeIORef unsafeCounter (id + 1)
    return id

idLabel :: ExprID -> String
idLabel id | id >= 0 = "x" ++ show id
           | otherwise = "y" ++ show (negate id)

class HasExprID a where
    getID :: a -> ExprID

