import Test.HUnit
import Control.Monad (when)
import System.Exit
import System.Directory (removeFile)
import qualified Data.ByteString as BS
import qualified Graphics.UI.GLUT as GLUT

import Graphics.HEGL
import Graphics.HEGL.Internal (dumpGlsl, hostEval)


-- Unit test setup

data GenericUnitTest d where
    GenericUnitTest :: String -> GLExpr d Bool -> GenericUnitTest d

-- verify that 'expr' evaluates to True
hostUnitTest :: GenericUnitTest HostDomain -> Test
hostUnitTest (GenericUnitTest label expr) = 
    TestLabel (label ++ "_host") $ TestCase $ do
        let ioev = error "Tests do not support evaluation of I/O GLExprs"
        res <- hostEval ioev expr
        assertBool ("Failed to evaluate to true on host: " ++ label) res

-- verify that setting color to 'expr' produces a white image
shaderUnitTest :: GenericUnitTest FragmentDomain -> Test
shaderUnitTest (GenericUnitTest label expr) = 
    TestLabel (label ++ "_shader") $ TestCase $ do
        let quadPos = vert 
                [(vec2 (-1) (-1)), 
                (vec2 (-1) 1), 
                (vec2 1 (-1)), 
                (vec2 1 1)]
            vPos = quadPos $- vec2 0 1
            color = cast expr .* 1
            obj = triangleStrip { position = vPos, color = color } 
            astDump = show expr
            glsl = dumpGlsl obj
        writeFile ("dist/test/test_" ++ label ++ ".dump") astDump
        writeFile ("dist/test/test_" ++ label ++ ".glsl") glsl
        let captureFile = "dist/test/test_" ++ label ++ ".ppm"
        -- TODO: use the same GLUT window instead of creating a new one every time
        GLUT.exit -- make sure GLUT has cleaned up, if a previous test errored
        -- TODO: calling drawGlut directly is inefficient 
        -- as it generates shader code a second time
        drawGlutCustom (defaultGlutOptions { 
            runMode = GlutCaptureAndExit captureFile }) obj
        dat <- BS.readFile captureFile
        let success = BS.all (== 0xff) . BS.drop (BS.length dat - 16) $ dat
        when success $ removeFile captureFile
        assertBool ("Failed to obtain a true value in shader: " ++ label) success


-- Unit tests

genericUnitTests :: [GenericUnitTest d]
genericUnitTests = [
        trivial,
        booleanExpr1,
        vec4Constr
    ]

trivial = GenericUnitTest "trivial" $
    true

booleanExpr1 = GenericUnitTest "boolean_expr1" $
    false .&& false .|| true .|| false .== true

vec4Constr = GenericUnitTest "vec4_constr" $
    let v = vec4 1 2 3 4 :: GLExpr d (Vec 4 Int)
    in v .== vec2 1 2 $- vec2 3 4 .&&
       v .== pre 1 (vec3 2 3 4) .&&
       v .== app (vec3 1 2 3) 4


-- TODO: test all of the following
--  boolean expressions
--  vectors and geometric functions
--  matrices and geometric functions
--  bitwise expressions
--  basic math expressions
--  accuracy of numerical computations
--  lifted functions
--  uniform == id
--  examples where caching makes a huge difference


runTests :: Test -> IO ()
runTests tests = do
    count <- runTestTT tests
    if errors count > 0 || failures count > 0 then exitFailure else return ()

main :: IO ()
main = do
    runTests $ TestList $ map hostUnitTest genericUnitTests
    runTests $ TestList $ map shaderUnitTest genericUnitTests
