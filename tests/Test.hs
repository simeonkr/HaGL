import Prelude hiding (all, min, max, sin, cos, sqrt, length)

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

data ExprTest d where
    ExprTest :: String -> GLExpr d Bool -> ExprTest d

data ObjTest =
    ObjTest String GLObj

-- verify that 'expr' evaluates to True
mkHostTest :: ExprTest HostDomain -> Test
mkHostTest (ExprTest label expr) = 
    TestLabel (label ++ "_host") $ TestCase $ do
        let ioev = error "Tests do not support evaluation of I/O GLExprs"
        res <- hostEval ioev expr
        assertBool ("Failed to evaluate to true on host: " ++ label) res

-- verify that setting color to 'expr' produces a white image
mkShaderTest :: ExprTest FragmentDomain -> Test
mkShaderTest (ExprTest label expr) = 
    mkObjTest $ ObjTest label $ objFromImage $ Prelude.const $ cast expr .* 1

-- verify that the given object produces a white image
mkObjTest :: ObjTest -> Test
mkObjTest (ObjTest label obj) = 
    TestLabel (label ++ "_shader") $ TestCase $ do
        let astDump = show (position obj) ++ "\n" ++ show (color obj)
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
        let success = BS.all (== 0xff) . BS.drop 16 $ dat
        when success $ removeFile captureFile
        assertBool ("Failed to obtain a true value in shader: " ++ label) success

objFromImage :: (FragExpr (Vec 2 Float) -> FragExpr (Vec 3 Float)) -> GLObj
objFromImage image = obj where
    quadPos = vert 
        [vec2 (-1) (-1), 
         vec2 (-1) 1, 
         vec2 1 (-1), 
         vec2 1 1]
    pos = quadPos $- vec2 0 1
    fPos = frag quadPos
    color = app (image fPos) 1
    obj = triangleStrip { position = pos, color = color }

-- TODO: make these definitions part of GLType
almostEqual x y = abs (x - y) .<= 1e-5
almostVecEqual x y = all $ lessThanEqual (abs (x - y)) 1e-5
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

genericTests :: [ExprTest d]
genericTests = [
        trivialTest,
        vec2Test,
        vec3Test,
        vec4Test,
        mat2Test,
        arrayTest,
        rawConstrTest,
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
        glFuncTrivial,
        vecArithmeticTest,
        trigIdentTest
    ]

hostTests :: [ExprTest HostDomain]
hostTests = [

    ]

shaderTests :: [ExprTest FragmentDomain]
shaderTests = [
        interpTest
    ]

objTests :: [ObjTest]
objTests = [
        trivialImageTest,
        passAroundTest
    ]


trivialTest = ExprTest "trivial" true


-- Vector and matrix (de-)construction and indexing

vec2Test = ExprTest "vec2" $
    let v = vec2 1 2 :: GLExpr d (Vec 2 Int)
        (decon -> (x, y)) = v
    in v .== 1 + vec2 0 1 .&&
       v .== 0 + 1 .* v + 0 .&&
       v .== vec2 1 0 + 2 .* vec2 0 1 .&&
       v .== vec2 (x_ v) (y_ v) .&&
       v .== vec2 x y .&&
       v .== (yx_ . yx_) v .&&
       v ./= (yx_ . xy_) v

vec3Test = ExprTest "vec3" $
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

vec4Test = ExprTest "vec4" $
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

mat2Test = ExprTest "mat2" $
    let m = mat2 (vec2 1 3) (vec2 2 4) :: GLExpr d (Mat 2 2 Float)
    in almostMatPx2Equal m (mat2 (col0 m) (col1 m))


-- Arrays

arrayTest = ExprTest "array" $
    let a1 = uniform $ array [Graphics.HEGL.const (2 * i) | i <- [0..999]] :: GLExpr d [Int]
        a2 = uniform $ array [vec2 1 1, vec2 2 2] :: GLExpr d [Vec 2 Int]
    in a1 .! 0 .== 0 .&& a1 .! 10 .== 2 * 10 .&&
       a1 .! 999 .== 2 * 999 .&&
       a1 .! (uniform 1) .== 2 .&&
       a2 .! 0 + a2 .! 0 .== a2 .! 1


-- Lifts from raw types

rawConstrTest = ExprTest "raw_constr" $
    let m0 = mat3 (vec3 1 4 7) (vec3 2 5 8) (vec3 3 6 9)
        m1 = uniform $ glLift0 $ fromList [1, 2, 3, 4, 5, 6, 7, 8, 9] :: GLExpr d (Mat 3 3 Float)
        m2 = uniform $ glLift0 $ fromMapping (\(i, j) -> fromIntegral $ 3 * i + j + 1) :: GLExpr d (Mat 3 3 Float)
        a0 = uniform $ array [1,2,3,4] :: GLExpr d [Int]
        a1 = uniform $ glLift0 [1,2,3,4] :: GLExpr d [Int]
        a2 = uniform $ glLift0 [4,3,2,1] :: GLExpr d [Int]
    in almostMatPx3Equal m0 m1 .&& almostMatPx3Equal m0 m2 .&& almostMatPx3Equal m1 m2 .&& 
       a0 .== a1 .&& a0 ./= a2

glLiftTest = ExprTest "glLift" $
    let f1 = glLift1 $ \x -> x + 1
        x1 = 1 :: GLExpr HostDomain Int
        f2 = glLift1 $ \x -> [x, x]
        x2 = 1 :: GLExpr HostDomain (Vec 2 Int)
        a2 = uniform $ array [x2, x2]
        f3 = glLift2 $ \x y -> [x, y, x + y]
        x3 = 1 :: GLExpr HostDomain Int
        y3 = 2 :: GLExpr HostDomain Int
        a3 = uniform $ array [1,2,3] :: GLExpr d [Int]
    in uniform (f1 x1) .== 2 .&& uniform (f2 x2) .== a2 .&& uniform (f3 x3 y3) .== a3


-- Type conversion



-- Boolean and bitwise expressions

booleanExprTest = ExprTest "boolean_expression1" $
    true .|| false .|| true .&& false .|| false .== true


-- Common math functions



-- Geometric functions

lengthTest = ExprTest "length" $
    almostEqual (length (vec3 2 (-3) 6 :: GLExpr d (Vec 3 Float))) 7 .&&
    let v@(decon -> (x, y, z, w)) = vec4 1 2 3 4 :: GLExpr d (Vec 4 Float)
    in almostEqual (length v) (sqrt (x * x + y * y + z * z + w * w))

distanceTest = ExprTest "distance" $
    let x = vec4 1 2 3 4 :: GLExpr d (Vec 4 Float)
        y = vec4 5 6 7 8 :: GLExpr d (Vec 4 Float)
    in almostEqual (distance x y) (length (x - y))

dotTest = ExprTest "dot" $
    let theta = 0.1234 :: GLExpr d Float
        x = vec2 (cos theta) (sin theta) :: GLExpr d (Vec 2 Float)
        y = vec2 (cos $ theta + (pi / 2)) (sin $ theta + (pi / 2))
        z = vec4 1 2 3 4 :: GLExpr d (Vec 4 Float)
    in almostEqual (dot x y) 0 .&&
       almostEqual (dot z z) (length z * length z)

crossTest = ExprTest "cross" $
    true

normalizeTest = ExprTest "normalize" $
    let x = vec4 1 2 3 4 :: GLExpr d (Vec 4 Float)
    in almostVecEqual (normalize x) ((1 / length x) .* x)

faceforwardTest = ExprTest "faceforward" $
    true

reflectTest = ExprTest "reflect" $
    true

refractTest = ExprTest "refract" $
    true


-- Matrix functions



-- Vector relational functions



-- Custom function support via glFunc

glFuncTrivial = ExprTest "glFunc_trivial" $
    let f = glFunc2 $ \x y -> x + y
        x0 = 1 :: GLExpr d Int
        x1 = 2 :: GLExpr d Int
    in f x0 x1 .== 3

glFuncNested = ExprTest "glFunc_nested" $
    undefined

glFuncRec = ExprTest "glFunc_rec" $
    undefined


-- uniform, prec, & builtin I/O variables



-- Shader-specific tests

trivialImageTest = ObjTest "trivial_image" $ objFromImage $ \pos ->
    max (app pos 1) 1

passAroundTest = ObjTest "pass_around" obj where
    ppos = vert 
        [vec2 (-1) (-1), 
         vec2 (-1) 1, 
         vec2 1 (-1), 
         vec2 1 1]
    npos = vert
        [vec2 1 1, 
         vec2 1 (-1), 
         vec2 (-1) 1, 
         vec2 (-1) (-1)]
    vpos = ppos $- vec2 0 1
    fppos = frag ppos
    fppos' = frag (-npos)
    fppos'' = -(frag npos)
    fnpos = frag npos
    fnpos' = frag (-ppos)
    fnpos'' = -(frag ppos)
    color = (cast $ 
        fppos   + fnpos   .== 0 .&& 
        fppos   + fnpos'  .== 0 .&&
        fppos   + fnpos'' .== 0 .&&
        fppos'  + fnpos   .== 0 .&& 
        fppos'  + fnpos'  .== 0 .&&
        fppos'  + fnpos'' .== 0 .&&
        fppos'' + fnpos   .== 0 .&& 
        fppos'' + fnpos'  .== 0 .&&
        fppos'' + fnpos'' .== 0) .* 1
    obj = triangleStrip { position = vpos, color = color }

interpTest = ExprTest "basic_interpolation_properties" $
    let x = vert [1, 2, 3, 4] :: VertExpr Float
        x' = vert $ map (\x -> 2 * x + 1) [1, 2, 3, 4]
        y = frag x
        z = 2 * noperspFrag x + 1
        z' = noperspFrag x'
        w = flatFrag x
    in y .>=1 .&& y .<= 4 .&& 
       almostEqual z z' .&& 
       (w .== 1 .|| w .== 2 .|| w .== 3 .|| w .== 4)

-- Other miscellaneous tests

vecArithmeticTest = ExprTest "vector_arithmetic" $
    2 .* vec4 1 2 3 4 - 3 * vec4 1 2 3 4 .== - (vec4 1 2 3 4 :: GLExpr d (Vec 4 Int)) .&&
    1 .* abs (vec4 1 1 1 1) - abs (-1) .== (vec4 0 0 0 0 :: GLExpr d (Vec 4 Int))

trigIdentTest = ExprTest "trigonometric_identities" $
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
    runTests $ TestList $ map mkObjTest objTests
