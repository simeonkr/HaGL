{-# OPTIONS_GHC -fno-cse -fno-full-laziness #-}

module Graphics.HEGL.ExprID (
    ExprID,
    genID,
    combineIDs,
    idLabel,
    HasExprID(..)
) where

import Prelude hiding (id)
import Data.Word (Word64)
import Data.Bits (shiftL, (.|.))
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.ByteString as BS (pack, unpack)
import qualified Crypto.Hash.MD5 as MD5

type ExprID = Word64

-- TODO: consider a hash consing approach which
-- would free us from shaky unsafePerformIO foundations,
-- at the expense of having ugly, long IDs in shader code

{-# NOINLINE unsafeCounter #-}
unsafeCounter :: IORef ExprID
unsafeCounter = unsafePerformIO $ newIORef 0

-- All modules using this function should be 
-- compiled with -fno-full-laziness!!
{-# NOINLINE genID #-}
genID :: a -> ExprID
genID _ = unsafePerformIO $ do
    id <- readIORef unsafeCounter
    writeIORef unsafeCounter (id + 1)
    return id

-- TODO: try to remove dependency on md5 and probabilistic assumptions
combineIDs :: [ExprID] -> ExprID
combineIDs = fromBytes . take 8 . map fromIntegral . BS.unpack . 
    MD5.hash . BS.pack . map fromIntegral where
        fromBytes = fst . foldr (\x (s, i) -> (s .|. shiftL x i, i + 8)) (0, 0)

idLabel :: ExprID -> String
idLabel id | id >= 0 = "x" ++ show id
           | otherwise = "y" ++ show (negate id)

class HasExprID a where
    getID :: a -> ExprID

