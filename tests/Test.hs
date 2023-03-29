import Prelude hiding (all, const, sin, cos, sqrt, length)

import Test.HUnit
import Control.Monad (when)
import System.Exit
import System.Directory (removeFile)
import qualified Data.ByteString as BS
import qualified Data.List as List
import qualified Graphics.UI.GLUT as GLUT

import Graphics.HEGL
import Graphics.HEGL.Internal (dumpGlsl, hostEval)


-- Test setup

data HEGLTest d where
    HEGLTest :: String -> GLExpr d Bool -> HEGLTest d

-- verify that 'expr' evaluates to True
mkHostTest :: HEGLTest HostDomain -> Test
mkHostTest (HEGLTest label expr) = 
    TestLabel (label ++ "_host") $ TestCase $ do
        let ioev = error "Tests do not support evaluation of I/O GLExprs"
        res <- hostEval ioev expr
        assertBool ("Failed to evaluate to true on host: " ++ label) res

-- verify that setting color to 'expr' produces a white image
mkShaderTest :: HEGLTest FragmentDomain -> Test
mkShaderTest (HEGLTest label expr) = 
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


almostEqual x y = abs (x - y) .<= 1e-12
almostVecEqual x y = all $ lessThanEqual (abs (x - y)) 1e-12
almostMatPx2Equal m1 m2 = 
    almostVecEqual (col0 m1) (col0 m2) .&&
    almostVecEqual (col1 m1) (col1 m2)
almostMatPx3Equal m1 m2 =
    almostVecEqual (col0 m1) (col0 m2) .&&
    almostVecEqual (col1 m1) (col1 m2) .&&
    almostVecEqual (col2 m1) (col2 m2)
almostMatPx4Equal m1 m2 =
    almostVecEqual (col0 m1) (col0 m2) .&&
    almostVecEqual (col1 m1) (col1 m2) .&&
    almostVecEqual (col2 m1) (col2 m2) .&&
    almostVecEqual (col3 m1) (col3 m2)


-- Tests

genericTests :: [HEGLTest d]
genericTests = [
        trivialTest,
        vec2Test,
        vec3Test,
        vec4Test,
        mat2Test,
        arrayTest,
        rawMatConstrTest,
        glLiftTest,
        booleanExprTest,
        lengthTest,
        distanceTest,
        dotTest,
        crossTest,
        normalizeTest,
        faceforwardTest,
        reflectTest,
        refractTest,
        vecArithmeticTest,
        trigIdentTest
    ]

hostTests :: [HEGLTest HostDomain]
hostTests = [

    ]

shaderTests :: [HEGLTest d]
shaderTests = [

    ]


trivialTest = HEGLTest "trivial" true


-- Vector and matrix (de-)construction and indexing

vec2Test = HEGLTest "vec2" $
    let v = vec2 1 2 :: GLExpr d (Vec 2 Int)
        (decon -> (x, y)) = v
    in v .== 1 + vec2 0 1 .&&
       v .== 0 + 1 .* v + 0 .&&
       v .== vec2 1 0 + 2 .* vec2 0 1 .&&
       v .== vec2 (x_ v) (y_ v) .&&
       v .== vec2 x y .&&
       v .== (yx_ . yx_) v .&&
       v ./= (yx_ . xy_) v

vec3Test = HEGLTest "vec3" $
    let v = vec3 1 2 3 :: GLExpr d (Vec 3 Int)
        (decon -> (x, y, z)) = v
    in v .== pre 1 (vec2 2 3) .&&
       v .== app (vec2 1 2) 3 .&&
       v .== 1 + vec3 0 1 2 .&&
       v .== 0 + 1.* v + 0 .&&
       v .== vec3 1 0 0 + 2 .* vec3 0 1 0 + 3 .* vec3 0 0 1 .&&
       v .== vec3 (x_ v) (y_ v) (z_ v) .&&
       v .== vec3 x y z .&&
       v .== pre x (vec2 y z) .&&
       v .== app (vec2 x y) z .&&
       v .== (zyx_ . zyx_) v .&&
       v ./= (zyx_ . xyz_) v

vec4Test = HEGLTest "vec4" $
    let v = vec4 1 2 3 4 :: GLExpr d (Vec 4 Int)
        (decon -> (x, y, z, w)) = v
    in v .== vec2 1 2 $- vec2 3 4 .&&
       v .== pre 1 (vec3 2 3 4) .&&
       v .== app (vec3 1 2 3) 4 .&&
       v .== 1 + vec4 0 1 2 3 .&&
       v .== 0 + 1 .* v + 0 .&&
       v .== vec4 1 0 0 0 + 2 .* vec4 0 1 0 0 + 
             3 .* vec4 0 0 1 0 + 4 .* vec4 0 0 0 1 .&&
       v .== vec4 (x_ v) (y_ v) (z_ v) (w_ v) .&&
       v .== vec4 x y z w .&&
       v .== pre x (vec3 y z w) .&&
       v .== app (vec3 x y z) w .&&
       v .== pre x ((zyx_ . wzy_) v) .&&
       v .== app ((xyz_ . xyz_) v) w

mat2Test = HEGLTest "mat2" $
    let m = mat2 (vec2 1 3) (vec2 2 4) :: GLExpr d (Mat 2 2 Float)
    in almostMatPx2Equal m (mat2 (col0 m) (col1 m))


-- Arrays

arrayTest = HEGLTest "array" $
    let a = uniform $ array [const (2 * i) | i <- [0..1000]] :: GLExpr d [Int]
    in a .! 0 .== 0 .&& a .! 10 .== 2 * 10 .&&
       a .! 999 .== 2 * 999


-- Lifts from raw types

rawMatConstrTest = HEGLTest "raw matrix constructors" $
    let m0 = mat3 (vec3 1 4 7) (vec3 2 5 8) (vec3 3 6 9)
        m1 = uniform $ glLift0 $ fromList [1, 2, 3, 4, 5, 6, 7, 8, 9] :: GLExpr d (Mat 3 3 Float)
        m2 = uniform $ glLift0 $ fromMapping (\(i, j) -> fromIntegral $ 3 * i + j + 1) :: GLExpr d (Mat 3 3 Float)
    in almostMatPx3Equal m0 m1 .&& almostMatPx3Equal m0 m2 .&& almostMatPx3Equal m1 m2

glLiftTest = HEGLTest "glLift" $
    let x = glLift0 [1,2,3,4] :: GLExpr HostDomain [Int]
        rx = glLift0 [4,3,2,1] :: GLExpr HostDomain [Int]
        ax = glLift0 [1,2,3,4,4,3,2,1] :: GLExpr HostDomain [Int]
        f1 = glLift1 List.reverse
        f2 = glLift2 (List.++)
    in uniform (f1 x) .== uniform rx .&&
       uniform (f2 x rx) .== uniform ax


-- Type conversion



-- Boolean and bitwise expressions

booleanExprTest = HEGLTest "boolean expression" $
    true .|| false .|| true .&& false .|| false .== true


-- Common math functions



-- Geometric functions

lengthTest = HEGLTest "length" $
    almostEqual (length (vec3 2 (-3) 6 :: GLExpr d (Vec 3 Float))) 7 .&&
    let v@(decon -> (x, y, z, w)) = vec4 1 2 3 4 :: GLExpr d (Vec 4 Float)
    in almostEqual (length v) (sqrt (x * x + y * y + z * z + w * w))

distanceTest = HEGLTest "distance" $
    let x = vec4 1 2 3 4 :: GLExpr d (Vec 4 Float)
        y = vec4 5 6 7 8 :: GLExpr d (Vec 4 Float)
    in almostEqual (distance x y) (length (x - y))

dotTest = HEGLTest "dot" $
    let theta = 0.1234 :: GLExpr d Float
        x = vec2 (cos theta) (sin theta) :: GLExpr d (Vec 2 Float)
        y = vec2 (cos $ theta + (pi / 2)) (sin $ theta + (pi / 2))
        z = vec4 1 2 3 4 :: GLExpr d (Vec 4 Float)
    in almostEqual (dot x y) 0 .&&
       almostEqual (dot z z) (length z * length z)

crossTest = HEGLTest "cross" $
    true

normalizeTest = HEGLTest "normalize" $
    let x = vec4 1 2 3 4 :: GLExpr d (Vec 4 Float)
    in almostVecEqual (normalize x) ((1 / length x) .* x)

faceforwardTest = HEGLTest "faceforward" $
    true

reflectTest = HEGLTest "reflect" $
    true

refractTest = HEGLTest "refact" $
    true


-- Matrix functions



-- Vector relational functions



-- Custom function support via glFunc



-- uniform, prec, & builtin I/O variables



-- Shader-specific tests: vert, frag



-- Other miscellaneous tests

vecArithmeticTest = HEGLTest "vector arithmetic" $
    2 .* vec4 1 2 3 4 - 3 * vec4 1 2 3 4 .== - (vec4 1 2 3 4 :: GLExpr d (Vec 4 Int)) .&&
    1 .* abs (vec4 1 1 1 1) - abs (-1) .== (vec4 0 0 0 0 :: GLExpr d (Vec 4 Int))

trigIdentTest = HEGLTest "trigonometric identities" $
    let x0 = 0.1234 :: GLExpr d Float
    in almostEqual (pow (sin x0) 2 + pow (cos x0) 2) 1


runTests :: Test -> IO ()
runTests tests = do
    count <- runTestTT tests
    if errors count > 0 || failures count > 0 then exitFailure else return ()

main :: IO ()
main = do
    runTests $ TestList $ map mkHostTest genericTests
    runTests $ TestList $ map mkHostTest hostTests
    runTests $ TestList $ map mkShaderTest genericTests
    runTests $ TestList $ map mkShaderTest shaderTests
