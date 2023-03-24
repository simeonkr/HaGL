{-# OPTIONS_GHC -fno-cse -fno-full-laziness #-}

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

-- All modules using this function should be compiled with 
-- -fno-full-laziness!!
{-# NOINLINE genID #-}
genID :: a -> ExprID
genID _ = unsafePerformIO $ do
    id <- readIORef unsafeCounter
    writeIORef unsafeCounter (id + 1)
    return id

idLabel :: ExprID -> String
idLabel id | id >= 0 = "x" ++ show id
           | otherwise = "y" ++ show (negate id)

class HasExprID a where
    getID :: a -> ExprID

