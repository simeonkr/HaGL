module Graphics.HEGL.ExprID (
    ExprID,
    genID,
    encodeUniquely,
    idLabel,
    HasExprID(..)
) where

import Data.IORef
import Data.Bits
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

-- encode an arbitrarily sized list of IDs
encodeUniquely :: [ExprID] -> ExprID
encodeUniquely xs = negate $ encodeTuple [length xs, encodeTuple xs]

-- FIXME: this approach won't scale and overflows are bound to occur; use a hash function
-- encode a fixed-size list of IDs by interleaving bits
encodeTuple :: [ExprID] -> ExprID
encodeTuple xs = encodeTuple' 0 (length xs) (maximum $ map bitsNeeded xs) xs where
    bitsNeeded x = finiteBitSize x - countLeadingZeros x
    encodeTuple' _ _ _ [] = 0
    encodeTuple' off stride n (x:xs) = 
        sum [placeBit i (off + stride * i) x | i <- [0..n-1]] + 
            encodeTuple' (off + 1) stride n xs
    placeBit i j x = shiftL (fromEnum $ testBit x i) j

idLabel :: ExprID -> String
idLabel id | id >= 0 = "x" ++ show id
           | otherwise = "y" ++ show (negate id)

class HasExprID a where
    getID :: a -> ExprID

