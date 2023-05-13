{-# LANGUAGE NumericUnderscores #-}

import Prelude hiding (all, min, max, length, floor)
import Test.HUnit
import Control.Monad (when)
import Control.Exception (Exception, try)
import System.Exit
import System.Directory (removeFile)
import qualified Data.ByteString as BS
import qualified Data.List as List
import qualified Graphics.UI.GLUT as GLUT

import Graphics.HEGL hiding (not, sin, cos, sqrt)
import Graphics.HEGL.Internal (dumpGlsl, hostEval, GLExprException(..), GLObjException(..))
import Graphics.HEGL.Lib.Image (fromImage)
import Graphics.HEGL.Examples


-- Test setup

data ExprTest d where
    ExprTest :: String -> GLExpr d Bool -> ExprTest d

data ExprExceptionTest =
    ExprExceptionTest String GLExprException (GLExpr FragmentDomain Bool)

data ObjTest =
    ObjTest String [GLObj]

data ObjExceptionTest where
    ObjExceptionTest :: (Exception e, Eq e) => String -> e -> [GLObj] -> ObjExceptionTest

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
    mkObjTest $ ObjTest label $ return $ fromImage $ \_ -> cast expr .# 1

-- verify that trying to set color to 'expr' throws the expected exception
mkShaderExceptionTest :: ExprExceptionTest -> Test
mkShaderExceptionTest (ExprExceptionTest label ex expr) =
    mkObjExceptionTest $ ObjExceptionTest label ex $ return $ fromImage $ \_ -> cast expr .# 1

-- verify that drawing the given objects produces a white image
mkObjTest :: ObjTest -> Test
mkObjTest (ObjTest label objs) = 
    TestLabel (label ++ "_shader") $ TestCase $
        runObjs False label objs >>=
            assertBool ("Failed to obtain a true value in shader: " ++ label)

-- verify that drawing the given objects throws the expected exception
mkObjExceptionTest :: ObjExceptionTest -> Test
mkObjExceptionTest (ObjExceptionTest label ex objs) =
    TestLabel label $ TestCase $ do
        res <- try $ runObjs False label objs
        case res of
            Left ex' | ex' == ex -> return ()
            _ -> assertFailure $ "Expected exception: " ++ show ex

runObjs :: Bool -> String -> [GLObj] -> IO Bool
runObjs alwaysSave label objs = do
    mapM_ (dumpObj label) (zip [0..] objs)
    let captureFile = "dist/test/test_" ++ label ++ ".ppm"
    -- TODO: use the same GLUT window instead of creating a new one every time
    GLUT.exit -- make sure GLUT has cleaned up, if a previous test errored
    -- TODO: calling drawGlut directly is inefficient 
    -- as it generates shader code a second time
    drawGlutCustom (defaultGlutOptions { 
        runMode = GlutCaptureAndExit captureFile }) objs
    dat <- BS.readFile captureFile
    let success = BS.all (== 0xff) . BS.drop 16 $ dat
    when (not alwaysSave && success) $ removeFile captureFile
    return success

dumpObj label (i, obj) = do
    let astDump = show (position obj) ++ "\n" ++ show (color obj)
        glsl = dumpGlsl obj
        objLabel = label ++ if i == 0 then "" else show i
    writeFile ("dist/test/test_" ++ objLabel ++ ".dump") astDump
    writeFile ("dist/test/test_" ++ objLabel ++ ".glsl") glsl

-- TODO: make these definitions part of GLType & use a smarter error estimate
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
        numFloatTest,
        numIntTest,
        numVecTest,
        numIntVecTest,
        fractionalFloatTest,
        fractionalVecTest,
        floatingFloatTest,
        floatingVecTest,
        numOpsIntTest,
        numOpsUIntTest,
        numOpsVecTest,
        numOpsIntVecTest,
        modulusTest,
        scalarMultTest,
        matMultTest,
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
        glFuncTrivialTest,
        glFuncMultipleTest,
        glFuncNestedTest,
        glFuncFactTest,
        glFuncFibTest,
        glFuncCollatzTest,
        glFuncMandelbrotTest,
        glFuncNestedCondTest,
        uniformFloatTest,
        uniformIntTest,
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
        inputFloatTest,
        inputIntTest,
        inputUIntTest,
        inputVec2Test,
        inputVec3Test,
        inputVec4Test,
        inputIntVec2Test,
        inputIntVec3Test,
        inputIntVec4Test,
        inputUIntVec2Test,
        inputUIntVec3Test,
        inputUIntVec4Test,
        interpTest,
        precTrivialTest,
        precNestedTest,
        precIntegrateTest,
        --precTimeTest,
        precSequenceTest
    ]

shaderExceptionTests :: [ExprExceptionTest]
shaderExceptionTests = [
        glLiftIllegalTest,
        glFuncFactIllegalTest,
        glFuncCollatzIllegalTest,
        glFuncNestedCondIllegalTest,
        glFuncNameCaptureIllegalTest,
        glFuncRecIllegalTest,
        glFuncMutRecIllegalTest
    ]

objTests :: [ObjTest]
objTests = [
        trivialImageTest,
        passAroundTest,
        multiObjOverlapTest,
        multiObjComplementTest,
        multiObjDiscardTest,
        multiObjSharedExprsTest
        --multiObjSharedHostExprsTest,
        --multiObjSharedPrecsTest
    ]

objExceptionTests :: [ObjExceptionTest]
objExceptionTests = [
        noInputVarsTest,
        emptyInputVarTest,
        mismatchedInputVarsTest
    ]


trivialTest = ExprTest "trivial" true


-- Vector and matrix (de-)construction and indexing

vec2Test = ExprTest "vec2" $
    let v = vec2 1 2 :: GLExpr d (Vec 2 Int)
        (decon -> (x, y)) = v
    in v .== 1 + vec2 0 1 .&&
       v .== 0 + 1 .# v + 0 .&&
       v .== vec2 1 0 + 2 .# vec2 0 1 .&&
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
       v .== 0 + 1.# v + 0 .&&
       v .== vec3 1 0 0 + 2 .# vec3 0 1 0 + 3 .# vec3 0 0 1 .&&
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
       v .== 0 + 1 .# v + 0 .&&
       v .== vec4 1 0 0 0 + 2 .# vec4 0 1 0 0 + 
             3 .# vec4 0 0 1 0 + 4 .# vec4 0 0 0 1 .&&
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
    let a1 = uniform $ array [cnst (2 * i) | i <- [0..999]] :: GLExpr d [Int]
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
    cast (uint 2) .== true .&&
    cast (uint 0) .== false .&&
    cast (1.7 :: GLExpr d Float) .== true .&&
    cast (0.7 :: GLExpr d Float) .== true .&&
    cast true .== (1 :: GLExpr d Int) .&&
    cast false .== (0 :: GLExpr d Int) .&&
    cast (uint 1) .== (1 :: GLExpr d Int) .&&
    cast (1.7 :: GLExpr d Float) .== (1 :: GLExpr d Int) .&&
    cast ((-1.7) :: GLExpr d Float) .== ((-1) :: GLExpr d Int) .&&
    almostEqual (cast true) (1 :: GLExpr d Float) .&&
    almostEqual (cast false) (0 :: GLExpr d Float) .&&
    almostEqual (cast (1 :: GLExpr d Int)) (1 :: GLExpr d Float) .&&
    almostEqual (cast (uint 1)) (1 :: GLExpr d Float)

matCastTest = ExprTest "matCast" $
    matCast (vec3 2 (-1) 0 :: GLExpr d (Vec 3 Int)) .== vec3 true true false .&&
    almostVecEqual (matCast $ vec2 true false) (vec2 1 0 :: GLExpr d (Vec 2 Float))


-- Num, Fractional, Floating

-- TODO: use QuickCheck to verify properties like these, wherever possible
checkNumProperties eq x y z =
    ((x + y) + z) `eq` (x + (y + z)) .&&
    (x + y) `eq` (y + x) .&&
    (x + fromInteger 0) `eq` x .&&
    (x + negate x) `eq` (fromInteger 0) .&&
    ((x * y) * z) `eq` (x * (y * z)) .&&
    (x * fromInteger 1) `eq` x .&&
    (fromInteger 1 * x) `eq` x .&&
    (x * (y + z)) `eq` ((x * y) + (x * z)) .&&
    ((y + z) * x) `eq` ((y * x) + (z * x)) .&&
    (abs x * signum x) `eq` x

numFloatTest = ExprTest "num_float" $
    checkNumProperties almostEqual (-1.234567 :: GLExpr d Float) 1.789012 (-1.567890)

numIntTest = ExprTest "num_int" $
    checkNumProperties (.==) (-1 :: GLExpr d Int) 7 (-5)

numVecTest = ExprTest "num_vec" $
    let v = vec4 1 2 3 4 :: GLExpr d (Vec 4 Float)
    in checkNumProperties almostVecEqual 
        (-1.234567 + v) (1.789012 + v) (-1.567890 + v)

numIntVecTest = ExprTest "num_ivec" $
    let v = vec4 1 2 3 4 :: GLExpr d (Vec 4 Int)
    in checkNumProperties (.==) 
        v (7 + v) (-5 + v)

checkFractionalProperties eq x y z = 
    (x * recip x) `eq` (recip x * x) .&&
    (x * recip x) `eq` (fromInteger 1) .&&
    (x * recip x) `eq` (fromRational (toRational 1)) .&&
    (x / y) `eq` (recip (y / x))

fractionalFloatTest = ExprTest "fractional_float" $
    checkFractionalProperties almostEqual (-1.234567 :: GLExpr d Float) 1.789012 (-1.567890)

fractionalVecTest = ExprTest "fractional_vec" $
    let v = vec4 1 2 3 4 :: GLExpr d (Vec 4 Float)
    in checkFractionalProperties almostVecEqual 
        (-1.234567 + v) (1.789012 + v) (-1.567890 + v)

checkFloatingProperties eq x y z = 
    (Prelude.exp (x + y)) `eq` (Prelude.exp x * Prelude.exp y) .&&
    (Prelude.exp (fromInteger 0)) `eq` (fromInteger 1) .&&
    (Prelude.log (abs $ x * y)) `eq` (Prelude.log (abs x) + Prelude.log (abs y)) .&&
    (Prelude.log (abs $ x / y)) `eq` (Prelude.log (abs x) - Prelude.log (abs y)) .&&
    (Prelude.sqrt (x ** 2)) `eq` (Prelude.abs x) .&&
    (x ** (-2)) `eq` recip (x ** 2) .&&
    ((Prelude.sin x ** 2) + (Prelude.cos x ** 2)) `eq` (fromInteger 1)

floatingFloatTest = ExprTest "floating_float" $
    checkFloatingProperties almostEqual (-1.234567 :: GLExpr d Float) 1.789012 (-1.567890)

floatingVecTest = ExprTest "floating_vec" $
    let v = vec4 0.1 0.2 0.3 0.4 :: GLExpr d (Vec 4 Float)
    in checkFloatingProperties almostVecEqual 
        (-1.234567 + v) (1.789012 + v) (-1.567890 + v)


-- Numeric operators

checkNumericOps eq zero one x y z =
    ((x .+ y) .+ z) `eq` (x .+ (y .+ z)) .&&
    (x .+ y) `eq` (y .+ x) .&&
    (x .+ zero) `eq` x .&&
    (x .+ neg x) `eq` zero .&&
    ((x .* y) .* z) `eq` (x .* (y .* z)) .&&
    (x .* one) `eq` x .&&
    (one .* x) `eq` x .&&
    (x .* (y .+ z)) `eq` ((x .* y) .+ (x .* z)) .&&
    ((y .+ z) .* x) `eq` ((y .* x) .+ (z .* x)) .&&
    (x ./ x) `eq` one .&&
    (x .* y ./ x) `eq` y .&&
    (x .* y ./ y) `eq` x

numOpsFloatTest = ExprTest "num_ops_float" $
    checkNumericOps almostEqual (0 :: GLExpr d Float) 1 (-1.234567) 1.789012 (-1.567890)

numOpsIntTest = ExprTest "num_ops_int" $
    checkNumericOps (.==) (0 :: GLExpr d Int) 1 (-1) 7 (-5) 

numOpsUIntTest = ExprTest "num_ops_uint" $
    checkNumericOps (.==) (uint 0) (uint 1) (uint 1) (uint 7) (uint 5)

numOpsVecTest = ExprTest "num_ops_vec" $
    let v = vec4 1 2 3 4 :: GLExpr d (Vec 4 Float)
    in checkNumericOps almostVecEqual 0 1
        (-1.234567 + v) (1.789012 + v) (-1.567890 + v)

numOpsIntVecTest = ExprTest "num_ops_ivec" $
    let v = vec4 1 2 3 4 :: GLExpr d (Vec 4 Int)
    in checkNumericOps (.==) 0 1
        v (7 + v) (-5 + v)

modulusTest = ExprTest "modulus" $
    vec4 10 100 101 200 .% (100 :: GLExpr d (Vec 4 Int)) .== vec4 10 0 1 0 .&&
    uint 101 .% uint 100 .== uint 1

scalarMultTest = ExprTest "scalar_mult" $
    (2 :: GLExpr d Int) .# vec2 1 2 .== vec2 2 4 .&&
    (2 :: GLExpr d Int) .# vec3 1 2 3 .== vec3 2 4 6 .&&
    (2 :: GLExpr d Int) .# vec4 1 2 3 4.== vec4 2 4 6 8 .&&
    ((2 :: GLExpr d Float) .# vec2 1 2) `almostVecEqual` vec2 2 4 .&&
    ((2 :: GLExpr d Float) .# vec3 1 2 3) `almostVecEqual` vec3 2 4 6 .&&
    ((2 :: GLExpr d Float) .# vec4 1 2 3 4) `almostVecEqual` vec4 2 4 6 8 .&&
    ((2 :: GLExpr d Float) .# mat2x2 (vec2 0 1) (vec2 2 3)) `almostMatPx2Equal` 
        mat2x2 (vec2 0 2) (vec2 4 6) .&&
    ((2 :: GLExpr d Float) .# mat2x3 (vec2 0 1) (vec2 2 3) (vec2 4 5)) `almostMatPx3Equal` 
        mat2x3 (vec2 0 2) (vec2 4 6) (vec2 8 10) .&&
    ((2 :: GLExpr d Float) .# mat2x4 (vec2 0 1) (vec2 2 3) (vec2 4 5) (vec2 6 7)) `almostMatPx4Equal` 
        mat2x4 (vec2 0 2) (vec2 4 6) (vec2 8 10) (vec2 12 14) .&&
    ((2 :: GLExpr d Float) .# mat3x2 (vec3 0 1 2) (vec3 3 4 5)) `almostMatPx2Equal` 
        mat3x2 (vec3 0 2 4) (vec3 6 8 10) .&&
    ((2 :: GLExpr d Float) .# mat3x3 (vec3 0 1 2) (vec3 3 4 5) (vec3 6 7 8)) `almostMatPx3Equal` 
        mat3x3 (vec3 0 2 4) (vec3 6 8 10) (vec3 12 14 16) .&&
    ((2 :: GLExpr d Float) .# mat3x4 (vec3 0 1 2) (vec3 3 4 5) (vec3 6 7 8) (vec3 9 10 11)) `almostMatPx4Equal` 
        mat3x4 (vec3 0 2 4) (vec3 6 8 10) (vec3 12 14 16) (vec3 18 20 22) .&&
    ((2 :: GLExpr d Float) .# mat4x2 (vec4 0 1 2 3) (vec4 4 5 6 7)) `almostMatPx2Equal` 
        mat4x2 (vec4 0 2 4 6) (vec4 8 10 12 14) .&&
    ((2 :: GLExpr d Float) .# mat4x3 (vec4 0 1 2 3) (vec4 4 5 6 7) (vec4 8 9 10 11)) `almostMatPx3Equal` 
        mat4x3 (vec4 0 2 4 6) (vec4 8 10 12 14) (vec4 16 18 20 22) .&&
    ((2 :: GLExpr d Float) .# mat4x4 (vec4 0 1 2 3) (vec4 4 5 6 7) (vec4 8 9 10 11) (vec4 12 13 14 15)) `almostMatPx4Equal` 
        mat4x4 (vec4 0 2 4 6) (vec4 8 10 12 14) (vec4 16 18 20 22) (vec4 24 26 28 30)

matMultTest = ExprTest "mat_mult" $
    let m2x2 = mat2x2 (vec2 0 1) (vec2 2 3) :: GLExpr d (Mat 2 2 Float)
        m2x3 = mat2x3 (vec2 0 1) (vec2 2 3) (vec2 4 5) :: GLExpr d (Mat 2 3 Float)
        m2x4 = mat2x4 (vec2 0 1) (vec2 2 3) (vec2 4 5) (vec2 6 7) :: GLExpr d (Mat 2 4 Float)
        m3x2 = mat3x2 (vec3 0 1 2) (vec3 3 4 5) :: GLExpr d (Mat 3 2 Float)
        m3x3 = mat3x3 (vec3 0 1 2) (vec3 3 4 5) (vec3 6 7 8) :: GLExpr d (Mat 3 3 Float)
        m3x4 = mat3x4 (vec3 0 1 2) (vec3 3 4 5) (vec3 6 7 8) (vec3 9 10 11) :: GLExpr d (Mat 3 4 Float)
        m4x2 = mat4x2 (vec4 0 1 2 3) (vec4 4 5 6 7) :: GLExpr d (Mat 4 2 Float)
        m4x3 = mat4x3 (vec4 0 1 2 3) (vec4 4 5 6 7) (vec4 8 9 10 11) :: GLExpr d (Mat 4 3 Float)
        m4x4 = mat4x4 (vec4 0 1 2 3) (vec4 4 5 6 7) (vec4 8 9 10 11) (vec4 12 13 14 15) :: GLExpr d (Mat 4 4 Float)
    in (m2x2 .@ m2x2) `almostMatPx2Equal` mat2x2 (vec2 2 3) (vec2 6 11) .&&
       (m2x4 .@ m4x3) `almostMatPx3Equal` mat2x3 (vec2 28 34) (vec2 76 98) (vec2 124 162) .&&
       (m2x3 .@ m3x4) `almostMatPx4Equal` mat2x4 (vec2 10 13) (vec2 28 40) (vec2 46 67) (vec2 64 94) .&&
       (m3x3 .@ m3x2) `almostMatPx2Equal` mat3x2 (vec3 15 18 21) (vec3 42 54 66) .&&
       (m3x2 .@ m2x3) `almostMatPx3Equal` mat3x3 (vec3 3 4 5) (vec3 9 14 19) (vec3 15 24 33) .&&
       (m3x4 .@ m4x4) `almostMatPx4Equal` mat3x4 (vec3 42 48 54) (vec3 114 136 158) (vec3 186 224 262) (vec3 258 312 366) .&&
       (m4x4 .@ m4x2) `almostMatPx2Equal` mat4x2 (vec4 56 62 68 74) (vec4 152 174 196 218) .&&
       (m4x3 .@ m3x3) `almostMatPx3Equal` mat4x3 (vec4 20 23 26 29) (vec4 56 68 80 92) (vec4 92 113 134 155) .&&
       (m4x2 .@ m2x4) `almostMatPx4Equal` mat4x4 (vec4 4 5 6 7) (vec4 12 17 22 27) (vec4 20 29 38 47) (vec4 28 41 54 67)


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
    nt true .== false .&&
    nt false .== true .&&
    cond true (2 :: GLExpr d Int) 1 .== 2 .&&
    cond false (2 :: GLExpr d Int) 1 .== 1    

bitwiseExprsTest = ExprTest "bitwise_expressions" $
    (((7 :: GLExpr d Int) .<< 3) .>> 3) .== 7 .&&
    ((7 :: GLExpr d Int) .>> 3) .== 0 .&&
    ((12 :: GLExpr d Int) .& 10) .== 8 .&&
    ((12 :: GLExpr d Int) .| 10) .== 14 .&&
    ((12 :: GLExpr d Int) .^ 10) .== 6 .&&
    (compl . compl) (12345678 :: GLExpr d Int) .== 12345678


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
    in almostVecEqual (normalize x) ((1 / length x) .# x)

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
    2 .# vec4 1 2 3 4 - 3 * vec4 1 2 3 4 .== - (vec4 1 2 3 4 :: GLExpr d (Vec 4 Int)) .&&
    1 .# abs (vec4 1 1 1 1) - abs (-1) .== (vec4 0 0 0 0 :: GLExpr d (Vec 4 Int))


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

-- currently unsupported
glFuncNameCaptureIllegalTest = ExprExceptionTest "glFunc_name_capture_illegal" UnsupportedNameCapture $
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
    let x = uint 4_000_000_000
    in uniform x .== x

uniformBoolTest = ExprTest "uniform_bool" $
    uniform true .== true .&& uniform false .== false

uniformVec2FloatTest = ExprTest "uniform_vec2" $
    let x = 1.234567 + vec2 1 2 :: GLExpr d (Vec 2 Float)
    in almostVecEqual (uniform x) x

uniformVec3FloatTest = ExprTest "uniform_vec3" $
    let x = 1.234567 + vec3 1 2 3 :: GLExpr d (Vec 3 Float)
    in almostVecEqual (uniform x) x

uniformVec4FloatTest = ExprTest "uniform_vec4" $
    let x = 1.234567 + vec4 1 2 3 4 :: GLExpr d (Vec 4 Float)
    in almostVecEqual (uniform x) x

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
    let x = vec2 (uint 4_000_000_000) (uint 4_000_000_001)
    in uniform x .== x

uniformVec3UIntTest = ExprTest "uniform_uvec3" $
    let x = vec3 (uint 4_000_000_001) (uint 4_000_000_002) (uint 4_000_000_003)
    in uniform x .== x

uniformVec4UIntTest = ExprTest "uniform_uvec4" $
    let x = vec4 (uint 4_000_000_001) (uint 4_000_000_002) (uint 4_000_000_003) (uint 4_000_000_004)
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
    in foldr (\i e -> e .&& almostEqual (uniform (array a) .! cnst i) (a !! (fromEnum i))) true [0..3]
    
uniformVec2ArrayTest = ExprTest "uniform_vec2[]" $
    let a = map (+ 0.2345) [vec2 1 2, vec2 3 4, vec2 5 6, vec2 7 8] :: [GLExpr d (Vec 2 Float)]
    in foldr (\i e -> e .&& almostVecEqual (uniform (array a) .! cnst i) (a !! (fromEnum i))) true [0..3]
    
uniformVec3ArrayTest = ExprTest "uniform_vec3[]" $
    let a = map (+ 0.2345) [vec3 1 2 3, vec3 4 5 6, vec3 7 8 9, vec3 10 11 12] :: [GLExpr d (Vec 3 Float)]
    in foldr (\i e -> e .&& almostVecEqual (uniform (array a) .! cnst i) (a !! (fromEnum i))) true [0..3]
    
uniformVec4ArrayTest = ExprTest "uniform_vec4[]" $
    let a = map (+ 0.2345) [vec4 1 2 3 4, vec4 5 6 7 8, vec4 9 10 11 12, vec4 13 14 15 16] :: [GLExpr d (Vec 4 Float)]
    in foldr (\i e -> e .&& almostVecEqual (uniform (array a) .! cnst i) (a !! (fromEnum i))) true [0..3]

uniformIntArrayTest = ExprTest "uniform_int[]" $
    let a = map (+ 2_000_000_000) [1, 2, 3, 4] :: [GLExpr d Int]
    in foldr (\i e -> e .&& uniform (array a) .! cnst i .== a !! (fromEnum i)) true [0..3]
    
uniformIntVec2ArrayTest = ExprTest "uniform_ivec2[]" $
    let a = map (+ 2_000_000_000) [vec2 1 2, vec2 3 4, vec2 5 6, vec2 7 8] :: [GLExpr d (Vec 2 Int)]
    in foldr (\i e -> e .&& uniform (array a) .! cnst i .== a !! (fromEnum i)) true [0..3]
    
uniformIntVec3ArrayTest = ExprTest "uniform_ivec3[]" $
    let a = map (+ 2_000_000_000) [vec3 1 2 3, vec3 4 5 6, vec3 7 8 9, vec3 10 11 12] :: [GLExpr d (Vec 3 Int)]
    in foldr (\i e -> e .&& uniform (array a) .! cnst i .== a !! (fromEnum i)) true [0..3]
    
uniformIntVec4ArrayTest = ExprTest "uniform_ivec4[]" $
    let a = map (+ 2_000_000_000) [vec4 1 2 3 4, vec4 5 6 7 8, vec4 9 10 11 12, vec4 13 14 15 16] :: [GLExpr d (Vec 4 Int)]
    in foldr (\i e -> e .&& uniform (array a) .! cnst i .== a !! (fromEnum i)) true [0..3]

uniformUIntArrayTest = ExprTest "uniform_uint[]" $
    let a = map (.+ (uint 4_000_000_000)) [uint 1, uint 2, uint 3, uint 4]
    in foldr (\i e -> e .&& uniform (array a) .! cnst i .== a !! (fromEnum i)) true [0..3]
    
uniformUIntVec2ArrayTest = ExprTest "uniform_uvec2[]" $
    let a = map (.+ (uint 4_000_000_000 .# vec2 (uint 1) (uint 1))) 
            [vec2 (uint 1) (uint 2), vec2 (uint 5) (uint 6), 
             vec2 (uint 9) (uint 10), vec2 (uint 13) (uint 14)]
    in foldr (\i e -> e .&& uniform (array a) .! cnst i .== a !! (fromEnum i)) true [0..3]
    
uniformUIntVec3ArrayTest = ExprTest "uniform_uvec3[]" $
    let a = map (.+ (uint 4_000_000_000 .# vec3 (uint 1) (uint 1) (uint 1))) 
            [vec3 (uint 1) (uint 2) (uint 3), vec3 (uint 5) (uint 6) (uint 7), 
             vec3 (uint 9) (uint 10) (uint 11), vec3 (uint 13) (uint 14) (uint 15)]
    in foldr (\i e -> e .&& uniform (array a) .! cnst i .== a !! (fromEnum i)) true [0..3]
    
uniformUIntVec4ArrayTest = ExprTest "uniform_uvec4[]" $
    let a = map (.+ (uint 4_000_000_000 .# vec4 (uint 1) (uint 1) (uint 1) (uint 1))) 
            [vec4 (uint 1) (uint 2) (uint 3) (uint 4), vec4 (uint 5) (uint 6) (uint 7) (uint 8), 
             vec4 (uint 9) (uint 10) (uint 11) (uint 12), vec4 (uint 13) (uint 14) (uint 15) (uint 16)]
    in foldr (\i e -> e .&& uniform (array a) .! cnst i .== a !! (fromEnum i)) true [0..3]

uniformBoolArrayTest = ExprTest "uniform_bool[]" $
    let a = [true, false, true, false] :: [GLExpr d Bool]
    in foldr (\i e -> e .&& uniform (array a) .! cnst i .== a !! (fromEnum i)) true [0..3]
    
uniformBoolVec2ArrayTest = ExprTest "uniform_bvec2[]" $
    let a = [vec2 true true, vec2 true false, vec2 false true, vec2 false false] :: [GLExpr d (Vec 2 Bool)]
    in foldr (\i e -> e .&& uniform (array a) .! cnst i .== a !! (fromEnum i)) true [0..3]
    
uniformBoolVec3ArrayTest = ExprTest "uniform_bvec3[]" $
    let a = [vec3 true true true, vec3 true true false, vec3 true false true, vec3 true false false] :: [GLExpr d (Vec 3 Bool)]
    in foldr (\i e -> e .&& uniform (array a) .! cnst i .== a !! (fromEnum i)) true [0..3]
    
uniformBoolVec4ArrayTest = ExprTest "uniform_bvec4[]" $
    let a = [vec4 true true true true, vec4 true true true false, vec4 true true false true, vec4 true true false false] :: [GLExpr d (Vec 4 Bool)]
    in foldr (\i e -> e .&& uniform (array a) .! cnst i .== a !! (fromEnum i)) true [0..3]

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
        tp = uniform $ array $ take 20 $ iterate (\t -> prec 0 t) t
    in foldr (\i e -> e .&& tp .! i .== uniform t - i) true (map cnst [0..19])


-- Shader-specific tests

-- TODO: test double-precision once supported

inputFloatTest = ExprTest "input_float" $
    let x = map (+ 0.2345) [1, 2, 3, 4] :: [GLExpr d Float]
    in almostEqual (frag (vert x)) (frag (1 + vert (map (\x -> x - 1) x)))

inputIntTest = ExprTest "input_int" $
    let x = map (+ 2_000_000_000) [1, 2, 3, 4] :: [GLExpr d Int]
    in flatFrag (vert x) .== flatFrag (1 + vert (map (\x -> x - 1) x))

inputUIntTest = ExprTest "input_uint" $
    let x = map (.+ (uint 4_000_000_000)) [uint 1, uint 2, uint 3, uint 4]
    in flatFrag (vert x) .== flatFrag (uint 1 .+ vert (map (\x -> x .- uint 1) x))

inputVec2Test = ExprTest "input_vec2" $
    let x = map (+ 0.2345) [vec2 1 2, vec2 3 4, vec2 5 6, vec2 7 8] :: [GLExpr d (Vec 2 Float)]
    in almostVecEqual (frag (vert x)) (frag (1 + vert (map (\x -> x - 1) x)))

inputVec3Test = ExprTest "input_vec3" $
    let x = map (+ 0.2345) [vec3 1 2 3, vec3 4 5 6, vec3 7 8 9, vec3 10 11 12] :: [GLExpr d (Vec 3 Float)]
    in almostVecEqual (frag (vert x)) (frag (1 + vert (map (\x -> x - 1) x)))

inputVec4Test = ExprTest "input_vec4" $
    let x = map (+ 0.2345) [vec4 1 2 3 4, vec4 5 6 7 8, vec4 9 10 11 12, vec4 13 14 15 16] :: [GLExpr d (Vec 4 Float)]
    in almostVecEqual (frag (vert x)) (frag (1 + vert (map (\x -> x - 1) x)))

inputIntVec2Test = ExprTest "input_ivec2" $
    let x = map (+ 2_000_000_000) [vec2 1 2, vec2 3 4, vec2 5 6, vec2 7 8] :: [GLExpr d (Vec 2 Float)]
    in flatFrag (vert x) .== flatFrag (1 + vert (map (\x -> x - 1) x))

inputIntVec3Test = ExprTest "input_ivec3" $
    let x = map (+ 2_000_000_000) [vec3 1 2 3, vec3 4 5 6, vec3 7 8 9, vec3 10 11 12] :: [GLExpr d (Vec 3 Float)]
    in flatFrag (vert x) .== flatFrag (1 + vert (map (\x -> x - 1) x))

inputIntVec4Test = ExprTest "input_ivec4" $
    let x = map (+ 2_000_000_000) [vec4 1 2 3 4, vec4 5 6 7 8, vec4 9 10 11 12, vec4 13 14 15 16] :: [GLExpr d (Vec 4 Float)]
    in flatFrag (vert x) .== flatFrag (1 + vert (map (\x -> x - 1) x))

inputUIntVec2Test = ExprTest "input_uvec2" $
    let x = map (+ 4_000_000_000) [vec2 1 2, vec2 3 4, vec2 5 6, vec2 7 8] :: [GLExpr d (Vec 2 Float)]
    in flatFrag (vert x) .== flatFrag (1 + vert (map (\x -> x - 1) x))

inputUIntVec3Test = ExprTest "input_uvec3" $
    let x = map (+ 4_000_000_000) [vec3 1 2 3, vec3 4 5 6, vec3 7 8 9, vec3 10 11 12] :: [GLExpr d (Vec 3 Float)]
    in flatFrag (vert x) .== flatFrag (1 + vert (map (\x -> x - 1) x))

inputUIntVec4Test = ExprTest "input_uvec4" $
    let x = map (+ 4_000_000_000) [vec4 1 2 3 4, vec4 5 6 7 8, vec4 9 10 11 12, vec4 13 14 15 16] :: [GLExpr d (Vec 4 Float)]
    in flatFrag (vert x) .== flatFrag (1 + vert (map (\x -> x - 1) x))

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


-- Object-level tests

trivialImageTest = ObjTest "trivial_image" $ return $ fromImage $ \pos ->
    vec4 1 1 1 1

quad = 
    [vec2 (-1) (-1), 
     vec2 (-1) 1, 
     vec2 1 (-1), 
     vec2 1 1]

passAroundTest = ObjTest "pass_around" [obj] where
    ppos = vert quad
    npos = vert $ map negate quad

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
        fppos'' + fnpos'' .== 0) .# 1
    obj = triangleStrip { position = vpos, color = color }

multiObjOverlapTest = ObjTest "multi_obj_overlap" [obj1, obj2] where
    vpos = vert quad
    obj1 = triangleStrip { position = vpos $- vec2 0 1, color = 0 }
    obj2 = triangleStrip { position = vpos $- vec2 0 1, color = 1 }

multiObjComplementTest = ObjTest "multi_obj_complement" [obj1, obj2] where
    vpos1 = vert 
        [vec2 (-1) 0, 
         vec2 (-1) 1, 
         vec2 1 0, 
         vec2 1 1]
    vpos2 = vert 
        [vec2 (-1) (-1), 
         vec2 (-1) 0, 
         vec2 1 (-1), 
         vec2 1 0]
    c1 = cast (y_ (frag vpos1) .>= 0) .# 1
    c2 = cast (y_ (frag vpos2) .< 0) .# 1
    obj1 = triangleStrip { position = vpos1 $- vec2 0 1, color = c1 }
    obj2 = triangleStrip { position = vpos2 $- vec2 0 1, color = c2 }

multiObjDiscardTest = ObjTest "multi_obj_discard" [obj1, obj2] where
    vpos = vert quad
    c1 = cast (y_ (frag vpos) .>= 0) .# 1
    d1 = y_ (frag vpos) .< 0
    c2 = cast (y_ (frag vpos) .< 0) .# 1
    d2 = y_ (frag vpos) .>= 0
    obj1 = triangleStrip { position = vpos $- vec2 0 1, color = c1, discardWhen = d1 }
    obj2 = triangleStrip { position = vpos $- vec2 0 1, color = c2, discardWhen = d2 }

multiObjSharedExprsTest = ObjTest "multi_obj_shared_exprs" [obj1, obj2] where
    vpos = vert quad
    c = 0.5 .# 1 + 0.5 .# 1
    d1 = y_ (frag vpos) .< 0
    d2 = y_ (frag vpos) .>= 0
    obj1 = triangleStrip { position = vpos $- vec2 0 1, color = c, discardWhen = d1 }
    obj2 = triangleStrip { position = vpos $- vec2 0 1, color = c, discardWhen = d2 }

-- TODO: the next two tests should be re-eningeered properly
-- the first is expected to fail and the second should pass
multiObjSharedHostExprsTest = ObjTest "multi_obj_shared_host_exprs" [obj1, obj2] where
    vpos = vert quad
    a = array [(cast . floor $ time * 100000000) 
            .% cnst (i + 1) | i <- [0..99]] :: HostExpr [Int]
    x = (uniform a .! ((cast $ 1000 * x_ (frag vpos)) .% 100)) .% 2 .== 0
    -- if obj1 and obj2 compute x separately then it is unlikely that
    -- c1 and c2 will agree
    c1 = cast x .# 1
    c2 = cast (nt x) .# 1
    d2 = x
    obj1 = triangleStrip { position = vpos $- vec2 0 1, color = c1 }
    obj2 = triangleStrip { position = vpos $- vec2 0 1, color = c2, discardWhen = d2 }

multiObjSharedPrecsTest = ObjTest "multi_obj_shared_precs" [obj1, obj2] where
    vpos = vert quad
    a = array [(cast . floor $ time * 100000000) 
            .% cnst (i + 1) | i <- [0..99]] :: HostExpr [Int]
    x = (uniform a .! ((cast $ 1000 * x_ (frag vpos)) .% 100)) .% 2 .== 0
    c1 = cast x .# 1
    c2 = cast (nt x) .# 1
    d2 = x
    obj1 = triangleStrip { position = vpos $- vec2 0 1, color = c1 }
    obj2 = triangleStrip { position = vpos $- vec2 0 1, color = c2, discardWhen = d2 }

noInputVarsTest = ObjExceptionTest "no_input_vars" NoInputVars $
    [triangleStrip]

emptyInputVarTest = ObjExceptionTest "empty_input_var" EmptyInputVar $
    [triangleStrip { position = vert [] }]

mismatchedInputVarsTest = ObjExceptionTest "mismatched_input_vars" MismatchedInputVars $
    [triangleStrip { position = vert [1, 1], color = frag (vert [1, 1, 1]) }]


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


-- Examples (verify output manually)

mkTestExample (label, objs) = TestLabel label $ TestCase $
    runObjs True label objs >>= (const $ assert True)

testExamples = 
    [("hello_triangles", [helloTriangles]),
     ("color_grad", [colorGrad]),
     ("circle", [circle]),
     ("vstrip", [vstrip]),
     ("circle_plus_strip", [circlePlusStrip]),
     ("circle_plus_strip2", circlePlusStrip'),
     ("checkboard", [checkboard]),
     ("rotating_checkboard", [rotatingCheckboard]),
     ("inverted_checkboard", [invertedCheckboard]),
     ("winding_path", [windingPath]),
     ("interactive_winding_path", [interactiveWindingPath]),
     ("frag_sphere", [fragSphere]),
     ("random_grid", [randomGrid]),
     ("noise_grid", [noiseGrid]),
     ("fractal_noise_grid", [fractalNoiseGrid]),
     ("warped_noise_grid", [warpedNoiseGrid]),
     ("procgen_2d_world", [procgen2dWorld]),
     ("mandelbrot", [mandelbrot]),
     ("particles", [particles]),
     ("particles2", [particles2]),
     ("pendulum", [pendulum]),
     ("doublePendulum", doublePendulum)
    ]


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
    runTests $ TestList $ map mkObjExceptionTest objExceptionTests
    runTests $ TestList $ map mkTestExample testExamples
