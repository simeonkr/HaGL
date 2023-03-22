module Graphics.HEGL.Test.Host where

import Test.HUnit
import Graphics.HEGL
import Graphics.HEGL.Internal (hostEval)


type HostTest = HostExpr Bool

hostTestCase :: String -> String -> HostTest -> Test
hostTestCase label descr test = TestLabel label $ TestCase $ do
    let ioev = error "Tests do not support evaluation of I/O GLExprs"
    res <- hostEval ioev test
    assertBool descr res


hostTests :: [Test]
hostTests = [
    hostTestCase "host_trivial_bool1" "trivial evaluation of boolean" trivial,
    hostTestCase "host_trivial_bool2" "trivial evaluation of boolean expression" trivialExpr
    ]

trivial = true

trivialExpr = let one = 1 :: HostExpr Int in Graphics.HEGL.not $ one ./= one
