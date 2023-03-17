module Graphics.HEGL.Test.Host where

import Test.HUnit
import Control.Monad (unless)
import Graphics.HEGL
import Graphics.HEGL.Internal (hostEval)


type HostTest = HostExpr Bool

hostTestCase :: String -> HostTest -> Test
hostTestCase msg test = TestCase $ do
    let ioev = error "Tests do not support evaluation of I/O GLExprs"
    res <- hostEval ioev test
    unless res (assertFailure msg)

hostTests :: [Test]
hostTests = [
    hostTestCase "trivial evaluation of boolean" trivial,
    hostTestCase "trivial evaluation of boolean expression" trivialExpr
    ]


trivial = true

trivialExpr = let one = 1 :: HostExpr Int in Graphics.HEGL.not $ one ./= one
