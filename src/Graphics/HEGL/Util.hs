module Graphics.HEGL.Util (
    warn
) where

import System.IO.Unsafe (unsafePerformIO)

warn :: String -> a -> a
warn msg res = unsafePerformIO $ do
    putStrLn msg
    return res
