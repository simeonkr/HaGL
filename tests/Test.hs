{-# LANGUAGE NumericUnderscores #-}

import Prelude hiding (const, all, not, min, max, sin, cos, sqrt, length)

import Test.HUnit
import Control.Monad (when)
import Control.Exception
import System.Exit
import System.Directory (removeFile)
import qualified Data.ByteString as BS
import qualified Data.List as List
import qualified Graphics.UI.GLUT as GLUT

import Graphics.HEGL
import Graphics.HEGL.Internal (dumpGlsl, hostEval, GLExprException(..))


-- Test setup

data ExprTest d where
    ExprTest :: String -> GLExpr d Bool -> ExprTest d

data ExprExceptionTest where
    ExprExceptionTest :: String -> GLExprException -> 
            GLExpr FragmentDomain Bool -> ExprExceptionTest

data ObjTest =
    ObjTest String GLObj

-- verify that 'expr' evaluates to True
mkHostTest :: ExprTest HostDomain -> Test
mkHostTest (ExprTest label expr) = 
    TestLabel (label ++ "_host") $ TestCase $ do
        let ioev = error "Host tests do not support evaluation of I/O GLExprs"
        res <- hostEval ioev expr
        assertBool ("Failed to evaluate to true on host: " ++ label) res

-- verify that setting color to 'expr' produces a white image
mkShaderTest :: ExprTest FragmentDomain -> Test
mkShaderTest (ExprTest label expr) = 
    mkObjTest $ ObjTest label $ objFromImage $ \_ -> cast expr .* 1

-- verify that trying to set color to 'expr' throws the expected exception
mkShaderExceptionTest :: ExprExceptionTest -> Test
mkShaderExceptionTest (ExprExceptionTest label ex expr) =
    TestLabel label $ TestCase $ do
        let (TestLabel _ (TestCase act)) = mkShaderTest (ExprTest label expr)
        res <- try act
        case res of
            Left ex' | ex' == ex -> return ()
            _ -> assertFailure $ "Expected exception: " ++ show ex

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
        mat3Test,
        mat4Test,
        mat2x3Test,
        mat2x3Test,
        mat3x2Test,
        mat3x4Test,
        mat4x2Test,
        mat4x3Test,
        arrayTest,
        rawConstrTest,
        glLiftTest,
        castTest,
        matCastTest,
        booleanExprsTest,
        bitwiseExprsTest,
        lengthTest,
        distanceTest,
        dotTest,
        crossTest,
        normalizeTest,
        faceforwardTest,
        reflectTest,
        refractTest,
        vecArithmeticTest,
        trigIdentTest,
        glFuncTrivialTest,
        glFuncMultipleTest,
        glFuncNestedTest,
        glFuncFactTest,
        glFuncFibTest,
        glFuncCollatzTest,
        glFuncMandelbrotTest,
        glFuncNestedCondTest,
        glFuncVarCaptureTest,
        uniformFloatTest,
        uniformUIntTest,
        uniformBoolTest,
        uniformVec2FloatTest,
        uniformVec3FloatTest,
        uniformVec4FloatTest,
        uniformVec2IntTest,
        uniformVec3IntTest,
        uniformVec4IntTest,
        uniformVec2UIntTest,
        uniformVec3UIntTest,
        uniformVec4UIntTest,
        uniformVec2BoolTest,
        uniformVec3BoolTest,
        uniformVec4BoolTest,
        uniformMat2Test,
        uniformMat3Test,
        uniformMat4Test,
        uniformMat2x3Test,
        uniformMat2x4Test,
        uniformMat3x2Test,
        uniformMat3x4Test,
        uniformMat4x2Test,
        uniformMat4x3Test,
        uniformFloatArrayTest,
        uniformVec2ArrayTest,
        uniformVec3ArrayTest,
        uniformVec4ArrayTest,
        uniformIntArrayTest,
        uniformIntVec2ArrayTest,
        uniformIntVec3ArrayTest,
        uniformIntVec4ArrayTest,
        uniformUIntArrayTest,
        uniformUIntVec2ArrayTest,
        uniformUIntVec3ArrayTest,
        uniformUIntVec4ArrayTest,
        uniformBoolArrayTest,
        uniformBoolVec2ArrayTest,
        uniformBoolVec3ArrayTest,
        uniformBoolVec4ArrayTest,
        exponentialExprTreeTest,
        iteratedGlFuncTest
    ]

hostTests :: [ExprTest HostDomain]
hostTests = [

    ]

shaderTests :: [ExprTest FragmentDomain]
shaderTests = [
        interpTest,
        precTrivialTest,
        precNestedTest,
        precIntegrateTest,
        --precTimeTest
        precSequenceTest
    ]

shaderExceptionTests :: [ExprExceptionTest]
shaderExceptionTests = [
        glLiftIllegalTest,
        glFuncFactIllegalTest,
        glFuncCollatzIllegalTest,
        glFuncNestedCondIllegalTest,
        glFuncRecIllegalTest,
        glFuncMutRecIllegalTest
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
    let m = mat2 (vec2 1 2) (vec2 3 4) :: GLExpr d (Mat 2 2 Float)
        (decon -> (c0, c1)) = m
    in almostMatPx2Equal m (mat2 (col0 m) (col1 m)) .&&
       almostMatPx2Equal m (mat2 c0 c1)

mat3Test = ExprTest "mat3" $
    let m = mat3 (vec3 1 2 3) (vec3 4 5 6) (vec3 7 8 9) :: GLExpr d (Mat 3 3 Float)
        (decon -> (c0, c1, c2)) = m
    in almostMatPx3Equal m (mat3 (col0 m) (col1 m) (col2 m)) .&&
       almostMatPx3Equal m (mat3 c0 c1 c2) {-.&&
       almostMatPx3Equal m ((mat3x2 c0 c1) $| c2) .&&
       almostMatPx3Equal m (c0 $| (mat3x2 c1 c2))-}

mat4Test = ExprTest "mat4" $
    let m = mat4 (vec4 1 2 3 4) (vec4 5 6 7 8) 
                 (vec4 9 10 11 12) (vec4 13 14 15 16) :: GLExpr d (Mat 4 4 Float)
        (decon -> (c0, c1, c2, c3)) = m
    in almostMatPx4Equal m (mat4 (col0 m) (col1 m) (col2 m) (col3 m)) .&&
       almostMatPx4Equal m (mat4 c0 c1 c2 c3) {-.&&
       almostMatPx4Equal m ((mat4x3 c0 c1 c2) $| c3) .&&
       almostMatPx4Equal m ((mat4x2 c0 c1) $| (mat4x2 c2 c3)) .&&
       almostMatPx4Equal m (c0 $| (mat4x3 c1 c2 c3))-}

mat2x3Test = ExprTest "mat2x3" $
    let m = mat2x3 (vec2 1 2) (vec2 3 4) (vec2 5 6) :: GLExpr d (Mat 2 3 Float)
        (decon -> (c0, c1, c2)) = m
    in almostMatPx3Equal m (mat2x3 (col0 m) (col1 m) (col2 m)) .&&
       almostMatPx3Equal m (mat2x3 c0 c1 c2)

mat2x4Test = ExprTest "mat2x4" $
    let m = mat2x4 (vec2 1 2) (vec2 3 4) (vec2 5 6) (vec2 7 8) :: GLExpr d (Mat 2 4 Float)
        (decon -> (c0, c1, c2, c3)) = m
    in almostMatPx4Equal m (mat2x4 (col0 m) (col1 m) (col2 m) (col3 m)) .&&
       almostMatPx4Equal m (mat2x4 c0 c1 c2 c3)

mat3x2Test = ExprTest "mat3x2" $
    let m = mat3x2 (vec3 1 2 3) (vec3 4 5 6) :: GLExpr d (Mat 3 2 Float)
        (decon -> (c0, c1)) = m
    in almostMatPx2Equal m (mat3x2 (col0 m) (col1 m)) .&&
       almostMatPx2Equal m (mat3x2 c0 c1)

mat3x4Test = ExprTest "mat3x4" $
    let m = mat3x4 (vec3 1 2 3) (vec3 4 5 6) (vec3 7 8 9) (vec3 10 11 12) :: GLExpr d (Mat 3 4 Float)
        (decon -> (c0, c1, c2, c3)) = m
    in almostMatPx4Equal m (mat3x4 (col0 m) (col1 m) (col2 m) (col3 m)) .&&
       almostMatPx4Equal m (mat3x4 c0 c1 c2 c3)

mat4x2Test = ExprTest "mat4x2" $
    let m = mat4x2 (vec4 1 2 3 4) (vec4 5 6 7 9) :: GLExpr d (Mat 4 2 Float)
        (decon -> (c0, c1)) = m
    in almostMatPx2Equal m (mat4x2 (col0 m) (col1 m)) .&&
       almostMatPx2Equal m (mat4x2 c0 c1)

mat4x3Test = ExprTest "mat4x3" $
    let m = mat4x3 (vec4 1 2 3 4) (vec4 5 6 7 8) (vec4 9 10 11 12) :: GLExpr d (Mat 4 3 Float)
        (decon -> (c0, c1, c2)) = m
    in almostMatPx3Equal m (mat4x3 (col0 m) (col1 m) (col2 m)) .&&
       almostMatPx3Equal m (mat4x3 c0 c1 c2)

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
        y1 = 2 :: GLExpr HostDomain Int
        f2 = glLift1 $ \x -> [x, x]
        x2 = 1 :: GLExpr HostDomain (Vec 2 Int)
        a2 = uniform $ array [x2, x2]
        f3 = glLift2 $ \x y -> [x, y, x + y]
        x3 = 1 :: GLExpr HostDomain Int
        y3 = 2 :: GLExpr HostDomain Int
        a3 = uniform $ array [1,2,3] :: GLExpr d [Int]
    in uniform (f1 x1) .== 2 .&& uniform (f1 y1) .== 3 .&& 
       uniform (f2 x2) .== a2 .&& uniform (f3 x3 y3) .== a3

glLiftIllegalTest = ExprExceptionTest "glLift_illegal" UnknownArraySize $
    let f = glLift1 $ \n -> replicate n 0
        a = uniform $ array [0, 0, 0] :: GLExpr d [Int]
    in uniform (f 3) .== a


-- Type conversion

castTest = ExprTest "cast" $
    cast (2 :: GLExpr d Int) .== true .&&
    cast ((-1) :: GLExpr d Int) .== true .&&
    cast (0 :: GLExpr d Int) .== false .&&
    cast (2 :: GLExpr d UInt) .== true .&&
    cast (0 :: GLExpr d UInt) .== false .&&
    cast (1.7 :: GLExpr d Float) .== true .&&
    cast (0.7 :: GLExpr d Float) .== true .&&
    cast true .== (1 :: GLExpr d Int) .&&
    cast false .== (0 :: GLExpr d Int) .&&
    cast (1 :: GLExpr d UInt) .== (1 :: GLExpr d Int) .&&
    cast (1.7 :: GLExpr d Float) .== (1 :: GLExpr d Int) .&&
    cast ((-1.7) :: GLExpr d Float) .== ((-1) :: GLExpr d Int) .&&
    almostEqual (cast true) (1 :: GLExpr d Float) .&&
    almostEqual (cast false) (0 :: GLExpr d Float) .&&
    almostEqual (cast (1 :: GLExpr d Int)) (1 :: GLExpr d Float) .&&
    almostEqual (cast (1 :: GLExpr d UInt)) (1 :: GLExpr d Float)

matCastTest = ExprTest "matCast" $
    matCast (vec3 2 (-1) 0 :: GLExpr d (Vec 3 Int)) .== vec3 true true false .&&
    almostVecEqual (matCast $ vec2 true false) (vec2 1 0 :: GLExpr d (Vec 2 Float))


-- Boolean and bitwise expressions

booleanExprsTest = ExprTest "boolean_expressions" $
    (-1 :: GLExpr d Int) .< 0 .&&
    (-1 :: GLExpr d Int) .<= 0 .&&
    (-1 :: GLExpr d Int) .<= -1 .&&
    (1 :: GLExpr d Int) .> 0 .&&
    (1 :: GLExpr d Int) .>= 0 .&&
    (1 :: GLExpr d Int) .>= 1 .&&
    (1 :: GLExpr d Int) .== 1 .&&
    (0 :: GLExpr d Int) ./= 1 .&&
    (true .&& true) .== true .&&
    (true .&& false) .== false .&&
    (false .&& true) .== false .&&
    (false .&& false) .== false .&&
    (true  .|| true) .== true .&&
    (true .|| false) .== true .&& 
    (false .|| true) .== true .&&
    (false .|| false) .== false .&&
    (true .^^ true) .== false .&&
    (true .^^ false) .== true .&&
    (false .^^ true) .== true .&&
    (false .^^ false) .== false .&&
    not true .== false .&&
    not false .== true .&&
    cond true (2 :: GLExpr d Int) 1 .== 2 .&&
    cond false (2 :: GLExpr d Int) 1 .== 1    


bitwiseExprsTest = ExprTest "bitwise_expressions" $
    (((7 :: GLExpr d Int) .<< 3) .>> 3) .== 7 .&&
    ((7 :: GLExpr d Int) .>> 3) .== 0 .&&
    ((12 :: GLExpr d Int) .& 10) .== 8 .&&
    ((12 :: GLExpr d Int) .| 10) .== 14 .&&
    ((12 :: GLExpr d Int) .^ 10) .== 6 .&&
    (neg . neg) (12345678 :: GLExpr d Int) .== 12345678


-- Num, Fractional, Floating

numTest = ExprTest "num_test" $
    true

numUnsignedTest = ExprTest "num_unsigned" $
    true

fractionalTest = ExprTest "fractional_test" $
    true

floatingTest = ExprTest "floating_test" $
    true


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



-- General numerical tests

vecArithmeticTest = ExprTest "vector_arithmetic" $
    2 .* vec4 1 2 3 4 - 3 * vec4 1 2 3 4 .== - (vec4 1 2 3 4 :: GLExpr d (Vec 4 Int)) .&&
    1 .* abs (vec4 1 1 1 1) - abs (-1) .== (vec4 0 0 0 0 :: GLExpr d (Vec 4 Int))

trigIdentTest = ExprTest "trigonometric_identities" $
    let x0 = 0.1234 :: GLExpr d Float
    in almostEqual (pow (sin x0) 2 + pow (cos x0) 2) 1


-- Custom function support via glFunc

glFuncTrivialTest = ExprTest "glFunc_trivial" $
    let f = glFunc2 $ \x y -> x + y
        x0 = 1 :: GLExpr d Int
        x1 = 2 :: GLExpr d Int
    in f x0 x1 .== 3

glFuncMultipleTest = ExprTest "glFunc_multiple" $
    let f1 :: GLExpr d Int -> GLExpr d Int
        f1 = glFunc1 (3 *)
        g1 :: GLExpr d Int -> GLExpr d Int
        g1 = glFunc1 (2 *)
        f2 :: GLExpr d Int -> GLExpr d Int -> GLExpr d Int
        f2 = glFunc2 $ \x y -> 2 * x * y
        g2 :: GLExpr d Int -> GLExpr d Int -> GLExpr d Int
        g2 = glFunc2 $ \x y -> 3 * x * y
    -- in the shader case, this should generate four non-main functions
    in f1 1 .> g1 1 .&& f1 1 .< g1 2 .&& f2 2 1 .> g2 1 1

glFuncNestedTest = ExprTest "glFunc_nested" $
    let f = glFunc2 $ \x y -> 2 * x + y
        g = glFunc3 $ \x y z -> z * f x y * (- f x y) * f y x
        x0 = 1 :: GLExpr d Int
        y0 = 2 :: GLExpr d Int
        z0 = 2 :: GLExpr d Int
    in g x0 y0 z0 .== -160

-- unsupported; flip arguments to cond as below
glFuncFactIllegalTest = ExprExceptionTest "glFunc_factorial_illegal" UnsupportedRecCall $ 
    let fact = glFunc1 $ \n -> fact' n 1
        fact' :: GLExpr d Int -> GLExpr d Int -> GLExpr d Int
        fact' = glFunc2 $ \n a -> cond (n ./= 0) (fact' (n - 1) (a * n)) a
    in fact 5 .== 120

glFuncFactTest = ExprTest "glFunc_factorial" $ 
    let fact = glFunc1 $ \n -> fact' n 1
        fact' :: GLExpr d Int -> GLExpr d Int -> GLExpr d Int
        fact' = glFunc2 $ \n a -> cond (n .== 0) a (fact' (n - 1) (a * n))
    in fact' 5 1 .== 120

glFuncFibTest = ExprTest "glFunc_fibonacci" $ 
    let fib = glFunc1 $ \n -> fib' n (vec2 0 1)
        fib' :: GLExpr d Int -> GLExpr d (Vec 2 Int) -> GLExpr d Int
        fib' = glFunc2 $ \n (decon -> (a, b)) -> cond (n .== 0) a (fib' (n - 1) (vec2 b (a + b)))
    in fib 6 .== 8

glFuncMandelbrotTest = ExprTest "glFunc_mandelbrot" $ 
    let mand :: GLExpr d (Vec 2 Float) -> GLExpr d (Vec 2 Float) -> GLExpr d Int -> GLExpr d Int
        mand = glFunc3 $ \pos0@(decon -> (x0, y0)) (decon -> (x, y)) i -> 
            cond (i .>= 50 .|| ((x * x + y * y) .> 4)) i $
                mand pos0 (vec2 (x * x - y * y + x0) (2 * x * y + y0)) (i + 1)
    in mand (vec2 3 0) (vec2 0 0) 0 .== 1 .&& 
       mand (vec2 0 0) (vec2 0 0) 0 .== 50

-- unsupported; canonicalize as in test below
glFuncCollatzIllegalTest = ExprExceptionTest "glFunc_collatz_illegal" UnsupportedRecCall $
    let f :: GLExpr d Int -> GLExpr d Int -> GLExpr d Int
        f = glFunc2 $ \n i -> cond (n .== 1) i $
            cond (n .% 2 .== 0) (f (n ./ 2) (i + 1)) (f (3 * n + 1) (i + 1))
    in f 27 0 .== 111

glFuncCollatzTest = ExprTest "glFunc_collatz" $
    let f :: GLExpr d Int -> GLExpr d Int -> GLExpr d Int
        f = glFunc2 $ \n i -> cond (n .== 1) i $
            f (cond (n .% 2 .== 0) (n ./ 2) (3 * n + 1)) (i + 1)
    in f 7 1 .== 17

-- unsupported; canonicalize as in test below
glFuncNestedCondIllegalTest = ExprExceptionTest "glFunc_nested_cond_illegal" UnsupportedRecCall $
    let f :: GLExpr d Int -> GLExpr d Int
        f = glFunc1 $ \n -> (1 +) $
            cond (n .== 1) 1 $
                cond (n .== 2) 2 $
                    cond (n .== 3) (f (n - 1)) $
                        cond  (n .== 4) (f (n - 3)) (f (n - 1))
    in f 1 .== 2 .&& f 2 .== 3 .&& f 3 .== 4 .&& f 4 .== 3 .&& f 10 .== 9

glFuncNestedCondTest = ExprTest "glFunc_nested_cond" $
    let f = glFunc1 $ \n -> f' n 0
        f' :: GLExpr d Int -> GLExpr d Int -> GLExpr d Int
        f' = glFunc2 $ \n a -> 
            cond (n .<= 4) (
                cond (n .== 1) (2 + a) $
                cond (n .== 2) (3 + a) $
                cond (n .== 3) (4 + a) (3 + a)
                ) $ f' (n - 1) (a + 1)
    in f 1 .== 2 .&& f 2 .== 3 .&& f 3 .== 4 .&& f 4 .== 3 .&& f 10 .== 9

glFuncVarCaptureTest = ExprTest "glFunc_var_capture" $
    let f :: GLExpr d Int -> GLExpr d Int -> GLExpr d Int
        f = glFunc2 $ \x u -> 
            let g = glFunc2 $ \y v -> x + y + u + v
            in g 2 0
    in f 1 0 .== 3

glFuncRecIllegalTest = ExprExceptionTest "glFunc_rec_call_illegal" UnsupportedRecCall $
    let f :: GLExpr d Int -> GLExpr d Int -> GLExpr d Int
        f = glFunc2 $ \x y -> f x (y + 1) + 1
    in f 0 0 .== 0

glFuncMutRecIllegalTest = ExprExceptionTest "glFunc_mut_rec_call_illegal" UnsupportedRecCall $
    let f :: GLExpr d Int -> GLExpr d Int
        f = glFunc1 $ \x -> g x + 1
        g = glFunc1 $ \x -> f x + 1
    in f 0 .== 0


-- uniform, prec, & builtin I/O variables

-- TODO: test double-precision once supported

uniformFloatTest = ExprTest "uniform_float" $
    almostEqual (uniform (1.2345 :: GLExpr d Float)) 1.2345

uniformIntTest = ExprTest "uniform_int" $
    let x = 2_000_000_000 :: GLExpr d Int
    in uniform x .== x

uniformUIntTest = ExprTest "uniform_uint" $
    let x = 4_000_000_000 :: GLExpr d UInt
    in uniform x .== x

uniformBoolTest = ExprTest "uniform_bool" $
    uniform true .== true .&& uniform false .== false

uniformVec2FloatTest = ExprTest "uniform_vec2" $
    let x = 1.234567 + vec2 1 2 :: GLExpr d (Vec 2 Float)
    in uniform x .== x

uniformVec3FloatTest = ExprTest "uniform_vec3" $
    let x = 1.234567 + vec3 1 2 3 :: GLExpr d (Vec 3 Float)
    in uniform x .== x

uniformVec4FloatTest = ExprTest "uniform_vec4" $
    let x = 1.234567 + vec4 1 2 3 4 :: GLExpr d (Vec 4 Float)
    in uniform x .== x

uniformVec2IntTest = ExprTest "uniform_ivec2" $
    let x = 2_000_000_000 + vec2 1 2 :: GLExpr d (Vec 2 Int)
    in uniform x .== x

uniformVec3IntTest = ExprTest "uniform_ivec3" $
    let x = 2_000_000_000 + vec3 1 2 3 :: GLExpr d (Vec 3 Int)
    in uniform x .== x

uniformVec4IntTest = ExprTest "uniform_ivec4" $
    let x = 2_000_000_000 + vec4 1 2 3 4 :: GLExpr d (Vec 4 Int)
    in uniform x .== x

uniformVec2UIntTest = ExprTest "uniform_uvec2" $
    let x = vec2 4_000_000_000 4_000_000_001 :: GLExpr d (Vec 2 UInt)
    in uniform x .== x

uniformVec3UIntTest = ExprTest "uniform_uvec3" $
    let x = 4_000_000_000 + vec3 1 2 3 :: GLExpr d (Vec 3 UInt)
    in uniform x .== x

uniformVec4UIntTest = ExprTest "uniform_uvec4" $
    let x = 4_000_000_000 + vec4 1 2 3 4 :: GLExpr d (Vec 4 UInt)
    in uniform x .== x

uniformVec2BoolTest = ExprTest "uniform_bvec2" $
    let x = vec2 true false :: GLExpr d (Vec 2 Bool)
    in uniform x .== x

uniformVec3BoolTest = ExprTest "uniform_bvec3" $
    let x = vec3 false true false :: GLExpr d (Vec 3 Bool)
    in uniform x .== x

uniformVec4BoolTest = ExprTest "uniform_bvec4" $
    let x = vec4 true false true false :: GLExpr d (Vec 4 Bool)
    in uniform x .== x

uniformMat2Test = ExprTest "uniform_mat2" $
    let m = mat2 (vec2 1 2) (vec2 3 4) :: GLExpr d (Mat 2 2 Float)
    in almostMatPx2Equal (uniform m) m

uniformMat3Test = ExprTest "uniform_mat3" $
    let m = mat3 (vec3 1 2 3) (vec3 4 5 6) (vec3 7 8 9) :: GLExpr d (Mat 3 3 Float)
    in almostMatPx3Equal (uniform m) m

uniformMat4Test = ExprTest "uniform_mat4" $
    let m = mat4 (vec4 1 2 3 4) (vec4 5 6 7 8) (vec4 9 10 11 12) (vec4 13 14 15 16) :: GLExpr d (Mat 4 4 Float)
    in almostMatPx4Equal (uniform m) m

uniformMat2x3Test = ExprTest "uniform_mat2x3" $
    let m = mat2x3 (vec2 1 2) (vec2 3 4) (vec2 5 6) :: GLExpr d (Mat 2 3 Float)
    in almostMatPx3Equal (uniform m) m

uniformMat2x4Test = ExprTest "uniform_mat2x4" $
    let m = mat2x4 (vec2 1 2) (vec2 3 4) (vec2 5 6) (vec2 7 8) :: GLExpr d (Mat 2 4 Float)
    in almostMatPx4Equal (uniform m) m

uniformMat3x2Test = ExprTest "uniform_mat3x2" $
    let m = mat3x2 (vec3 1 2 3) (vec3 4 5 6) :: GLExpr d (Mat 3 2 Float)
    in almostMatPx2Equal (uniform m) m

uniformMat3x4Test = ExprTest "uniform_mat3x4" $
    let m = mat3x4 (vec3 1 2 3) (vec3 4 5 6) (vec3 7 8 9) (vec3 10 11 12) :: GLExpr d (Mat 3 4 Float)
    in almostMatPx4Equal (uniform m) m

uniformMat4x2Test = ExprTest "uniform_mat4x2" $
    let m = mat4x2 (vec4 1 2 3 4) (vec4 5 6 7 8) :: GLExpr d (Mat 4 2 Float)
    in almostMatPx2Equal (uniform m) m

uniformMat4x3Test = ExprTest "uniform_mat4x3" $
    let m = mat4x3 (vec4 1 2 3 4) (vec4 5 6 7 8) (vec4 9 10 11 12) :: GLExpr d (Mat 4 3 Float)
    in almostMatPx3Equal (uniform m) (mat4x3 (vec4 1 2 3 4) (vec4 5 6 7 8) (vec4 9 10 11 12))

uniformFloatArrayTest = ExprTest "uniform_float[]" $
    let a = map (+ 0.2345) [1, 2, 3, 4] :: [GLExpr d Float]
    in foldr (\i e -> e .&& almostEqual (uniform (array a) .! const i) (a !! (fromEnum i))) true [0..3]
    
uniformVec2ArrayTest = ExprTest "uniform_vec2[]" $
    let a = map (+ 0.2345) [vec2 1 2, vec2 3 4, vec2 5 6, vec2 7 8] :: [GLExpr d (Vec 2 Float)]
    in foldr (\i e -> e .&& almostVecEqual (uniform (array a) .! const i) (a !! (fromEnum i))) true [0..3]
    
uniformVec3ArrayTest = ExprTest "uniform_vec3[]" $
    let a = map (+ 0.2345) [vec3 1 2 3, vec3 4 5 6, vec3 7 8 9, vec3 10 11 12] :: [GLExpr d (Vec 3 Float)]
    in foldr (\i e -> e .&& almostVecEqual (uniform (array a) .! const i) (a !! (fromEnum i))) true [0..3]
    
uniformVec4ArrayTest = ExprTest "uniform_vec4[]" $
    let a = map (+ 0.2345) [vec4 1 2 3 4, vec4 5 6 7 8, vec4 9 10 11 12, vec4 13 14 15 16] :: [GLExpr d (Vec 4 Float)]
    in foldr (\i e -> e .&& almostVecEqual (uniform (array a) .! const i) (a !! (fromEnum i))) true [0..3]

uniformIntArrayTest = ExprTest "uniform_int[]" $
    let a = map (+ 2_000_000_000) [1, 2, 3, 4] :: [GLExpr d Int]
    in foldr (\i e -> e .&& uniform (array a) .! const i .== a !! (fromEnum i)) true [0..3]
    
uniformIntVec2ArrayTest = ExprTest "uniform_ivec2[]" $
    let a = map (+ 2_000_000_000) [vec2 1 2, vec2 3 4, vec2 5 6, vec2 7 8] :: [GLExpr d (Vec 2 Int)]
    in foldr (\i e -> e .&& uniform (array a) .! const i .== a !! (fromEnum i)) true [0..3]
    
uniformIntVec3ArrayTest = ExprTest "uniform_ivec3[]" $
    let a = map (+ 2_000_000_000) [vec3 1 2 3, vec3 4 5 6, vec3 7 8 9, vec3 10 11 12] :: [GLExpr d (Vec 3 Int)]
    in foldr (\i e -> e .&& uniform (array a) .! const i .== a !! (fromEnum i)) true [0..3]
    
uniformIntVec4ArrayTest = ExprTest "uniform_ivec4[]" $
    let a = map (+ 2_000_000_000) [vec4 1 2 3 4, vec4 5 6 7 8, vec4 9 10 11 12, vec4 13 14 15 16] :: [GLExpr d (Vec 4 Int)]
    in foldr (\i e -> e .&& uniform (array a) .! const i .== a !! (fromEnum i)) true [0..3]

uniformUIntArrayTest = ExprTest "uniform_uint[]" $
    let a = map (+ 4_000_000_000) [1, 2, 3, 4] :: [GLExpr d UInt]
    in foldr (\i e -> e .&& uniform (array a) .! const i .== a !! (fromEnum i)) true [0..3]
    
uniformUIntVec2ArrayTest = ExprTest "uniform_uvec2[]" $
    let a = map (+ 4_000_000_000) [vec2 1 2, vec2 3 4, vec2 5 6, vec2 7 8] :: [GLExpr d (Vec 2 UInt)]
    in foldr (\i e -> e .&& uniform (array a) .! const i .== a !! (fromEnum i)) true [0..3]
    
uniformUIntVec3ArrayTest = ExprTest "uniform_uvec3[]" $
    let a = map (+ 4_000_000_000) [vec3 1 2 3, vec3 4 5 6, vec3 7 8 9, vec3 10 11 12] :: [GLExpr d (Vec 3 UInt)]
    in foldr (\i e -> e .&& uniform (array a) .! const i .== a !! (fromEnum i)) true [0..3]
    
uniformUIntVec4ArrayTest = ExprTest "uniform_uvec4[]" $
    let a = map (+ 4_000_000_000) [vec4 1 2 3 4, vec4 5 6 7 8, vec4 9 10 11 12, vec4 13 14 15 16] :: [GLExpr d (Vec 4 UInt)]
    in foldr (\i e -> e .&& uniform (array a) .! const i .== a !! (fromEnum i)) true [0..3]

uniformBoolArrayTest = ExprTest "uniform_bool[]" $
    let a = [true, false, true, false] :: [GLExpr d Bool]
    in foldr (\i e -> e .&& uniform (array a) .! const i .== a !! (fromEnum i)) true [0..3]
    
uniformBoolVec2ArrayTest = ExprTest "uniform_bvec2[]" $
    let a = [vec2 true true, vec2 true false, vec2 false true, vec2 false false] :: [GLExpr d (Vec 2 Bool)]
    in foldr (\i e -> e .&& uniform (array a) .! const i .== a !! (fromEnum i)) true [0..3]
    
uniformBoolVec3ArrayTest = ExprTest "uniform_bvec3[]" $
    let a = [vec3 true true true, vec3 true true false, vec3 true false true, vec3 true false false] :: [GLExpr d (Vec 3 Bool)]
    in foldr (\i e -> e .&& uniform (array a) .! const i .== a !! (fromEnum i)) true [0..3]
    
uniformBoolVec4ArrayTest = ExprTest "uniform_bvec4[]" $
    let a = [vec4 true true true true, vec4 true true true false, vec4 true true false true, vec4 true true false false] :: [GLExpr d (Vec 4 Bool)]
    in foldr (\i e -> e .&& uniform (array a) .! const i .== a !! (fromEnum i)) true [0..3]

precTrivialTest = ExprTest "prec_trivial" $
    let x = prec (0 :: GLExpr d Int) x
        x1 = prec (0 :: GLExpr d Int) (x1 + 1)
        y1 = prec (0 :: GLExpr d Int) (y1 + 1)
    in uniform x .== 0 .&& uniform x1 .> 0 .&& uniform x1 .== uniform y1

precNestedTest = ExprTest "prec_nested" $
    let x = prec (0 :: GLExpr d Int) (x + 1)
        y = prec (-1 :: GLExpr d Int) x
    in uniform y .> 0

precIntegrateTest = ExprTest "prec_integrate" $
    let t = prec (0 :: GLExpr d Int) (t + 1)
        x = prec (0 :: GLExpr d Int) (x + t)
    in uniform t .> 0 .&&
       2 * uniform x .== uniform t * (uniform t - 1)

precTimeTest = ExprTest "prec_time" $
    let dt = prec 0 (time - prec 0 time)
        dt_sum = prec 0 (dt_sum + dt)
    in almostEqual (uniform $ prec 0 time) (uniform dt_sum)

precSequenceTest = ExprTest "prec_sequence" $
    let t = prec (0 :: GLExpr d Int) (t + 1)
        tp = uniform $ array $ take 20 $ iterate (prec t) t
    in tp .! 19 .== uniform t - 19


-- Shader-specific tests

-- TODO: test vert for all GLInputTypes

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


-- Caching tests

exponentialExprTreeTest = ExprTest "exponential_expr_tree" $
    let f x = x + x 
        h = 40
    -- this is infeasible without caching as the entire
    -- expression tree of size 2^h will have to be traversed
    in iterate f (1 :: GLExpr d Int) !! h .== 2^h

iteratedGlFuncTest = ExprTest "iterated_glFunc" $
    let f = glFunc1 $ \x -> x + x 
        h = 40
    in iterate f (1 :: GLExpr d Int) !! h .== 2^h



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
    runTests $ TestList $ map mkShaderExceptionTest shaderExceptionTests
    runTests $ TestList $ map mkObjTest objTests
