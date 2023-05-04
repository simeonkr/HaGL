{-# LANGUAGE ScopedTypeVariables #-}

module Graphics.HEGL.GLType (
    UInt,
    Vec,
    Mat,
    GLType(..),
    GLPrimOrVec,
    GLInputType(..),
    GLSupportsSmoothInterp,
    GLSupportsBitwiseOps,
    GLElt,
    GLPrim(..), 
    GLSingle, 
    GLNumeric,
    GLSigned,
    GLFloating, 
    GLSingleNumeric, 
    GLInteger,
    genDiv
) where

import Control.Applicative (liftA2, liftA3)
import Data.Bits
import Data.Word (Word32)
import Data.List (intercalate)
import Foreign.Storable (Storable)
import Foreign.Marshal.Array (withArray)
import qualified Graphics.Rendering.OpenGL as OpenGL
import qualified Graphics.GL as RawGL

import Graphics.HEGL.Numerical


-- * Raw types

type UInt = Word32

class (Eq t, Show t) => GLType t where
    showGlslType :: a t -> String
    showGlslVal :: t -> String
    glMap :: (GLElt t -> GLElt t) -> t -> t
    glZipWith :: (GLElt t -> GLElt t -> GLElt t) -> t -> t -> t
    glZipWith3 :: (GLElt t -> GLElt t -> GLElt t -> GLElt t) -> t -> t -> t -> t
    eltSize :: [t] -> Int
    numComponents :: [t] -> Int
    arrayLen :: t -> Int
    getGlslType :: [t] -> OpenGL.DataType
    uniformSet :: OpenGL.GLint -> t -> IO ()

instance GLType Float where
    showGlslType = const "float"
    showGlslVal = show
    glMap = id
    glZipWith = id
    glZipWith3 = id
    eltSize = const 4
    numComponents = const 1
    arrayLen = const 1
    getGlslType = const OpenGL.Float
    uniformSet = RawGL.glUniform1f
instance GLType Double where
    showGlslType = const "double"
    showGlslVal = show
    glMap = id
    glZipWith = id
    glZipWith3 = id
    eltSize = const 8
    numComponents = const 1
    arrayLen = const 1
    getGlslType = const OpenGL.Double
    uniformSet = RawGL.glUniform1d
instance GLType Int where
    showGlslType = const "int"
    showGlslVal = show
    glMap = id
    glZipWith = id
    glZipWith3 = id
    eltSize = const 4
    numComponents = const 1
    arrayLen = const 1
    getGlslType = const OpenGL.Int
    uniformSet i x = RawGL.glUniform1i i (toEnum x)
instance GLType UInt where
    showGlslType = const "uint"
    showGlslVal x = show x ++ "u"
    glMap = id
    glZipWith = id
    glZipWith3 = id
    eltSize = const 4
    numComponents = const 1
    arrayLen = const 1
    getGlslType = const OpenGL.UnsignedInt
    uniformSet = RawGL.glUniform1ui
instance GLType Bool where
    showGlslType = const "bool"
    showGlslVal x = case x of True -> "true"; False -> "false"
    glMap = id
    glZipWith = id
    glZipWith3 = id
    eltSize = const 1
    numComponents = const 1
    arrayLen = const 1
    getGlslType = const OpenGL.Byte
    uniformSet i x = RawGL.glUniform1i i (fromBool x)
instance GLType (Vec 2 Float) where
    showGlslType = const "vec2"
    showGlslVal v = printVec "vec2" (toList v)
    glMap = fmap
    glZipWith = liftA2
    glZipWith3 = liftA3
    eltSize = const 4
    numComponents = const 2
    arrayLen = const 1
    getGlslType = const OpenGL.Float
    uniformSet ul v = RawGL.glUniform2f ul 
        (v `eltAt` (0, 0)) (v `eltAt` (1, 0))
instance GLType (Vec 3 Float) where
    showGlslType = const "vec3"
    showGlslVal v = printVec "vec3" (toList v)
    glMap = fmap
    glZipWith = liftA2
    glZipWith3 = liftA3
    eltSize = const 4
    numComponents = const 3
    arrayLen = const 1
    getGlslType = const OpenGL.Float
    uniformSet ul v = RawGL.glUniform3f ul 
        (v `eltAt` (0, 0)) (v `eltAt` (1, 0)) (v `eltAt` (2, 0))
instance GLType (Vec 4 Float) where
    showGlslType = const "vec4"
    showGlslVal v = printVec "vec4" (toList v)
    glMap = fmap
    glZipWith = liftA2
    glZipWith3 = liftA3
    eltSize = const 4
    numComponents = const 4
    arrayLen = const 1
    getGlslType = const OpenGL.Float
    uniformSet ul v = RawGL.glUniform4f ul 
        (v `eltAt` (0, 0)) (v `eltAt` (1, 0)) (v `eltAt` (2, 0)) (v `eltAt` (3, 0))
instance GLType (Vec 2 Double) where
    showGlslType = const "dvec2"
    showGlslVal v = printVec "dvec2" (toList v)
    glMap = fmap
    glZipWith = liftA2
    glZipWith3 = liftA3
    eltSize = const 4
    numComponents = const 2
    arrayLen = const 1
    getGlslType = const OpenGL.Double
    uniformSet ul v = RawGL.glUniform2d ul 
        (v `eltAt` (0, 0)) (v `eltAt` (1, 0))
instance GLType (Vec 3 Double) where
    showGlslType = const "dvec3"
    showGlslVal v = printVec "dvec3" (toList v)
    glMap = fmap
    glZipWith = liftA2
    glZipWith3 = liftA3
    eltSize = const 4
    numComponents = const 3
    arrayLen = const 1
    getGlslType = const OpenGL.Double
    uniformSet ul v = RawGL.glUniform3d ul 
        (v `eltAt` (0, 0)) (v `eltAt` (1, 0)) (v `eltAt` (2, 0))
instance GLType (Vec 4 Double) where
    showGlslType = const "dvec4" 
    showGlslVal v = printVec "dvec4" (toList v)
    glMap = fmap
    glZipWith = liftA2
    glZipWith3 = liftA3
    eltSize = const 4
    numComponents = const 4
    arrayLen = const 1
    getGlslType = const OpenGL.Double
    uniformSet ul v = RawGL.glUniform4d ul 
        (v `eltAt` (0, 0)) (v `eltAt` (1, 0)) (v `eltAt` (2, 0)) (v `eltAt` (3, 0))
instance GLType (Vec 2 Int) where
    showGlslType = const "ivec2"
    showGlslVal v = printVec "ivec2" (toList v)
    glMap = fmap
    glZipWith = liftA2
    glZipWith3 = liftA3
    eltSize = const 4
    numComponents = const 2
    arrayLen = const 1
    getGlslType = const OpenGL.Int
    uniformSet ul v = RawGL.glUniform2i ul 
        (toEnum $ v `eltAt` (0, 0)) (toEnum $ v `eltAt` (1, 0))
instance GLType (Vec 3 Int) where
    showGlslType = const "ivec3"
    showGlslVal v = printVec "ivec3" (toList v)
    glMap = fmap
    glZipWith = liftA2
    glZipWith3 = liftA3
    eltSize = const 4
    numComponents = const 3
    arrayLen = const 1
    getGlslType = const OpenGL.Int
    uniformSet ul v = RawGL.glUniform3i ul 
        (toEnum $ v `eltAt` (0, 0)) (toEnum $ v `eltAt` (1, 0)) (toEnum $ v `eltAt` (2, 0))
instance GLType (Vec 4 Int) where
    showGlslType = const "ivec4" 
    showGlslVal v = printVec "ivec4" (toList v)
    glMap = fmap
    glZipWith = liftA2
    glZipWith3 = liftA3
    eltSize = const 4
    numComponents = const 4
    arrayLen = const 1
    getGlslType = const OpenGL.Int
    uniformSet ul v = RawGL.glUniform4i ul 
        (toEnum $ v `eltAt` (0, 0)) (toEnum $ v `eltAt` (1, 0)) (toEnum $ v `eltAt` (2, 0)) (toEnum $ v `eltAt` (3, 0))
instance GLType (Vec 2 UInt) where
    showGlslType = const "uvec2"
    showGlslVal v = printVec "uvec2" (toList v)
    glMap = fmap
    glZipWith = liftA2
    glZipWith3 = liftA3
    eltSize = const 4
    numComponents = const 2
    arrayLen = const 1
    getGlslType = const OpenGL.UnsignedInt
    uniformSet ul v = RawGL.glUniform2ui ul 
        (v `eltAt` (0, 0)) (v `eltAt` (1, 0))
instance GLType (Vec 3 UInt) where
    showGlslType = const "uvec3"
    showGlslVal v = printVec "uvec3" (toList v)
    glMap = fmap
    glZipWith = liftA2
    glZipWith3 = liftA3
    eltSize = const 4
    numComponents = const 3
    arrayLen = const 1
    getGlslType = const OpenGL.UnsignedInt
    uniformSet ul v = RawGL.glUniform3ui ul 
        (v `eltAt` (0, 0)) (v `eltAt` (1, 0)) (v `eltAt` (2, 0))
instance GLType (Vec 4 UInt) where
    showGlslType = const "uvec4"
    showGlslVal v = printVec "uvec4" (toList v)
    glMap = fmap
    glZipWith = liftA2
    glZipWith3 = liftA3
    eltSize = const 4
    numComponents = const 4
    arrayLen = const 1
    getGlslType = const OpenGL.UnsignedInt
    uniformSet ul v = RawGL.glUniform4ui ul 
        (v `eltAt` (0, 0)) (v `eltAt` (1, 0)) (v `eltAt` (2, 0)) (v `eltAt` (3, 0))
instance GLType (Vec 2 Bool) where
    showGlslType = const "bvec2"
    showGlslVal v = printVec "bvec2" (toList v)
    glMap = fmap
    glZipWith = liftA2
    glZipWith3 = liftA3
    eltSize = const 4
    numComponents = const 2
    arrayLen = const 1
    getGlslType = const OpenGL.Byte
    uniformSet ul v = RawGL.glUniform2i ul 
        (fromBool $ v `eltAt` (0, 0)) (fromBool $ v `eltAt` (1, 0))
instance GLType (Vec 3 Bool) where
    showGlslType = const "bvec3"
    showGlslVal v = printVec "bvec3" (toList v)
    glMap = fmap
    glZipWith = liftA2
    glZipWith3 = liftA3
    eltSize = const 4
    numComponents = const 3
    arrayLen = const 1
    getGlslType = const OpenGL.Byte
    uniformSet ul v = RawGL.glUniform3i ul 
        (fromBool $ v `eltAt` (0, 0)) (fromBool $ v `eltAt` (1, 0)) (fromBool $ v `eltAt` (2, 0))
instance GLType (Vec 4 Bool) where
    showGlslType = const "bvec4" 
    showGlslVal v = printVec "bvec4" (toList v)
    glMap = fmap
    glZipWith = liftA2
    glZipWith3 = liftA3
    eltSize = const 4
    numComponents = const 4
    arrayLen = const 1
    getGlslType = const OpenGL.Byte
    uniformSet ul v = RawGL.glUniform4i ul 
        (fromBool $ v `eltAt` (0, 0)) (fromBool $ v `eltAt` (1, 0)) (fromBool $ v `eltAt` (2, 0)) (fromBool $ v `eltAt` (3, 0))
instance GLType (Mat 2 2 Float) where
    showGlslVal m = printVec "mat2x2" (toList $ transpose m)
    showGlslType = const "mat2x2"
    glMap = fmap
    glZipWith = liftA2
    glZipWith3 = liftA3
    eltSize = const 4
    numComponents = const 4
    arrayLen = const 1
    getGlslType = const OpenGL.Float
    uniformSet ul m = 
        makeMatSetter RawGL.glUniformMatrix2fv ul (toList $ transpose m)
instance GLType (Mat 2 3 Float) where
    showGlslVal m = printVec "mat3x2" (toList $ transpose m)
    showGlslType = const "mat3x2"
    glMap = fmap
    glZipWith = liftA2
    glZipWith3 = liftA3
    eltSize = const 4
    numComponents = const 6
    arrayLen = const 1
    getGlslType = const OpenGL.Float
    uniformSet ul m = 
        makeMatSetter RawGL.glUniformMatrix3x2fv ul (toList $ transpose m)
instance GLType (Mat 2 4 Float) where
    showGlslVal m = printVec "mat4x2" (toList $ transpose m)
    showGlslType = const "mat4x2"
    glMap = fmap
    glZipWith = liftA2
    glZipWith3 = liftA3
    eltSize = const 4
    numComponents = const 8
    arrayLen = const 1
    getGlslType = const OpenGL.Float
    uniformSet ul m = 
        makeMatSetter RawGL.glUniformMatrix4x2fv ul (toList $ transpose m)
instance GLType (Mat 3 2 Float) where
    showGlslVal m = printVec "mat2x3" (toList $ transpose m)
    showGlslType = const "mat2x3"
    glMap = fmap
    glZipWith = liftA2
    glZipWith3 = liftA3
    eltSize = const 4
    numComponents = const 6
    arrayLen = const 1
    getGlslType = const OpenGL.Float
    uniformSet ul m = 
        makeMatSetter RawGL.glUniformMatrix2x3fv ul (toList $ transpose m)
instance GLType (Mat 3 3 Float) where
    showGlslVal m = printVec "mat3x3" (toList $ transpose m)
    showGlslType = const "mat3x3"
    glMap = fmap
    glZipWith = liftA2
    glZipWith3 = liftA3
    eltSize = const 4
    numComponents = const 9
    arrayLen = const 1
    getGlslType = const OpenGL.Float
    uniformSet ul m = 
        makeMatSetter RawGL.glUniformMatrix3fv ul (toList $ transpose m)
instance GLType (Mat 3 4 Float) where
    showGlslVal m = printVec "mat4x3" (toList $ transpose m)
    showGlslType = const "mat4x3"
    glMap = fmap
    glZipWith = liftA2
    glZipWith3 = liftA3
    eltSize = const 4
    numComponents = const 12
    arrayLen = const 1
    getGlslType = const OpenGL.Float
    uniformSet ul m = 
        makeMatSetter RawGL.glUniformMatrix4x3fv ul (toList $ transpose m)
instance GLType (Mat 4 2 Float) where
    showGlslVal m = printVec "mat2x4" (toList $ transpose m)
    showGlslType = const "mat2x4"
    glMap = fmap
    glZipWith = liftA2
    glZipWith3 = liftA3
    eltSize = const 4
    numComponents = const 8
    arrayLen = const 1
    getGlslType = const OpenGL.Float
    uniformSet ul m = 
        makeMatSetter RawGL.glUniformMatrix2x4fv ul (toList $ transpose m)
instance GLType (Mat 4 3 Float) where
    showGlslVal m = printVec "mat3x4" (toList $ transpose m)
    showGlslType = const "mat3x4"
    glMap = fmap
    glZipWith = liftA2
    glZipWith3 = liftA3
    eltSize = const 4
    numComponents = const 12
    arrayLen = const 1
    getGlslType = const OpenGL.Float
    uniformSet ul m = 
        makeMatSetter RawGL.glUniformMatrix3x4fv ul (toList $ transpose m)
instance GLType (Mat 4 4 Float) where
    showGlslVal m = printVec "mat4x4" (toList $ transpose m)
    showGlslType = const "mat4x4"
    glMap = fmap
    glZipWith = liftA2
    glZipWith3 = liftA3
    eltSize = const 4
    numComponents = const 16
    arrayLen = const 1
    getGlslType = const OpenGL.Float
    uniformSet ul m = 
        makeMatSetter RawGL.glUniformMatrix4fv ul (toList $ transpose m)
instance GLType (Mat 2 2 Double) where
    showGlslVal m = printVec "dmat2x2" (toList $ transpose m)
    showGlslType = const "dmat2x2"
    glMap = fmap
    glZipWith = liftA2
    glZipWith3 = liftA3
    eltSize = const 8
    numComponents = const 4
    arrayLen = const 1
    getGlslType = const OpenGL.Double
    uniformSet ul m = 
        makeMatSetter RawGL.glUniformMatrix2dv ul (toList $ transpose m)
instance GLType (Mat 2 3 Double) where
    showGlslVal m = printVec "dmat3x2" (toList $ transpose m)
    showGlslType = const "dmat3x2"
    glMap = fmap
    glZipWith = liftA2
    glZipWith3 = liftA3
    eltSize = const 8
    numComponents = const 6
    arrayLen = const 1
    getGlslType = const OpenGL.Double
    uniformSet ul m = 
        makeMatSetter RawGL.glUniformMatrix3x2dv ul (toList $ transpose m)
instance GLType (Mat 2 4 Double) where
    showGlslVal m = printVec "dmat4x2" (toList $ transpose m)
    showGlslType = const "dmat4x2"
    glMap = fmap
    glZipWith = liftA2
    glZipWith3 = liftA3
    eltSize = const 8
    numComponents = const 8
    arrayLen = const 1
    getGlslType = const OpenGL.Double
    uniformSet ul m = 
        makeMatSetter RawGL.glUniformMatrix4x2dv ul (toList $ transpose m)
instance GLType (Mat 3 2 Double) where
    showGlslVal m = printVec "dmat2x3" (toList $ transpose m)
    showGlslType = const "dmat2x3"
    glMap = fmap
    glZipWith = liftA2
    glZipWith3 = liftA3
    eltSize = const 8
    numComponents = const 6
    arrayLen = const 1
    getGlslType = const OpenGL.Double
    uniformSet ul m = 
        makeMatSetter RawGL.glUniformMatrix2x3dv ul (toList $ transpose m)
instance GLType (Mat 3 3 Double) where
    showGlslVal m = printVec "dmat3x3" (toList $ transpose m)
    showGlslType = const "dmat3x3"
    glMap = fmap
    glZipWith = liftA2
    glZipWith3 = liftA3
    eltSize = const 8
    numComponents = const 9
    arrayLen = const 1
    getGlslType = const OpenGL.Double
    uniformSet ul m = 
        makeMatSetter RawGL.glUniformMatrix3dv ul (toList $ transpose m)
instance GLType (Mat 3 4 Double) where
    showGlslVal m = printVec "dmat4x3" (toList $ transpose m)
    showGlslType = const "dmat4x3"
    glMap = fmap
    glZipWith = liftA2
    glZipWith3 = liftA3
    eltSize = const 8
    numComponents = const 12
    arrayLen = const 1
    getGlslType = const OpenGL.Double
    uniformSet ul m = 
        makeMatSetter RawGL.glUniformMatrix4x3dv ul (toList $ transpose m)
instance GLType (Mat 4 2 Double) where
    showGlslVal m = printVec "dmat2x4" (toList $ transpose m)
    showGlslType = const "dmat2x4"
    glMap = fmap
    glZipWith = liftA2
    glZipWith3 = liftA3
    eltSize = const 8
    numComponents = const 8
    arrayLen = const 1
    getGlslType = const OpenGL.Double
    uniformSet ul m = 
        makeMatSetter RawGL.glUniformMatrix2x4dv ul (toList $ transpose m)
instance GLType (Mat 4 3 Double) where
    showGlslVal m = printVec "dmat3x4" (toList $ transpose m)
    showGlslType = const "dmat3x4"
    glMap = fmap
    glZipWith = liftA2
    glZipWith3 = liftA3
    eltSize = const 8
    numComponents = const 12
    arrayLen = const 1
    getGlslType = const OpenGL.Double
    uniformSet ul m = 
        makeMatSetter RawGL.glUniformMatrix3x4dv ul (toList $ transpose m)
instance GLType (Mat 4 4 Double) where
    showGlslVal m = printVec "dmat4x4" (toList $ transpose m)
    showGlslType = const "dmat4x4"
    glMap = fmap
    glZipWith = liftA2
    glZipWith3 = liftA3
    arrayLen = const 1
    eltSize = const 8
    numComponents = const 16
    getGlslType = const OpenGL.Double
    uniformSet ul m = 
        makeMatSetter RawGL.glUniformMatrix4dv ul (toList $ transpose m)
instance GLType [Float] where
    showGlslVal xs = "float[](" ++ intercalate ", " (map showGlslVal xs) ++ ")"
    showGlslType = const "float[]"
    glMap = fmap
    glZipWith = liftA2
    glZipWith3 = liftA3
    arrayLen = Prelude.length
    eltSize = const 4
    numComponents = const 1
    getGlslType = const OpenGL.Float
    uniformSet ul xs = 
        withArray xs $ RawGL.glUniform1fv ul (fromIntegral $ Prelude.length xs)
instance GLType [Vec 2 Float] where
    showGlslVal xs = "vec2[](" ++ intercalate ", " (map showGlslVal xs) ++ ")"
    showGlslType = const "vec2[]"
    glMap = fmap
    glZipWith = liftA2
    glZipWith3 = liftA3
    arrayLen = Prelude.length
    eltSize = const 4
    numComponents = const 2
    getGlslType = const OpenGL.Float
    uniformSet ul xs = let xs' = concatMap toList xs in
        withArray xs' $ RawGL.glUniform2fv ul (fromIntegral $ Prelude.length xs')
instance GLType [Vec 3 Float] where
    showGlslVal xs = "vec3[](" ++ intercalate ", " (map showGlslVal xs) ++ ")"
    showGlslType = const "vec3[]"
    glMap = fmap
    glZipWith = liftA2
    glZipWith3 = liftA3
    arrayLen = Prelude.length
    eltSize = const 4
    numComponents = const 3
    getGlslType = const OpenGL.Float
    uniformSet ul xs = let xs' = concatMap toList xs in
        withArray xs' $ RawGL.glUniform3fv ul (fromIntegral $ Prelude.length xs')
instance GLType [Vec 4 Float] where
    showGlslVal xs = "vec4[](" ++ intercalate ", " (map showGlslVal xs) ++ ")"
    showGlslType = const "vec4[]"
    glMap = fmap
    glZipWith = liftA2
    glZipWith3 = liftA3
    arrayLen = Prelude.length
    eltSize = const 4
    numComponents = const 4
    getGlslType = const OpenGL.Float
    uniformSet ul xs = let xs' = concatMap toList xs in
        withArray xs' $ RawGL.glUniform4fv ul (fromIntegral $ Prelude.length xs')
instance GLType [Double] where
    showGlslVal xs = "double[](" ++ intercalate ", " (map showGlslVal xs) ++ ")"
    showGlslType = const "double[]"
    glMap = fmap
    glZipWith = liftA2
    glZipWith3 = liftA3
    arrayLen = Prelude.length
    eltSize = const 8
    numComponents = const 1
    getGlslType = const OpenGL.Double
    uniformSet ul xs = 
        withArray xs $ RawGL.glUniform1dv ul (fromIntegral $ Prelude.length xs)
instance GLType [Vec 2 Double] where
    showGlslVal xs = "dvec2[](" ++ intercalate ", " (map showGlslVal xs) ++ ")"
    showGlslType = const "dvec2[]"
    glMap = fmap
    glZipWith = liftA2
    glZipWith3 = liftA3
    arrayLen = Prelude.length
    eltSize = const 8
    numComponents = const 2
    getGlslType = const OpenGL.Double
    uniformSet ul xs = let xs' = concatMap toList xs in
        withArray xs' $ RawGL.glUniform2dv ul (fromIntegral $ Prelude.length xs')
instance GLType [Vec 3 Double] where
    showGlslVal xs = "dvec3[](" ++ intercalate ", " (map showGlslVal xs) ++ ")"
    showGlslType = const "dvec3[]"
    glMap = fmap
    glZipWith = liftA2
    glZipWith3 = liftA3
    arrayLen = Prelude.length
    eltSize = const 8
    numComponents = const 3
    getGlslType = const OpenGL.Double
    uniformSet ul xs = let xs' = concatMap toList xs in
        withArray xs' $ RawGL.glUniform3dv ul (fromIntegral $ Prelude.length xs')
instance GLType [Vec 4 Double] where
    showGlslVal xs = "dvec4[](" ++ intercalate ", " (map showGlslVal xs) ++ ")"
    showGlslType = const "dvec4[]"
    glMap = fmap
    glZipWith = liftA2
    glZipWith3 = liftA3
    arrayLen = Prelude.length
    eltSize = const 8
    numComponents = const 4
    getGlslType = const OpenGL.Double
    uniformSet ul xs = let xs' = concatMap toList xs in
        withArray xs' $ RawGL.glUniform4dv ul (fromIntegral $ Prelude.length xs')
instance GLType [Int] where
    showGlslVal xs = "int[](" ++ intercalate ", " (map showGlslVal xs) ++ ")"
    showGlslType = const "int[]"
    glMap = fmap
    glZipWith = liftA2
    glZipWith3 = liftA3
    arrayLen = Prelude.length
    eltSize = const 4
    numComponents = const 1
    getGlslType = const OpenGL.Int
    uniformSet ul xs = let xs' = map toEnum xs in
            withArray xs' $ RawGL.glUniform1iv ul (fromIntegral $ Prelude.length xs')
instance GLType [Vec 2 Int] where
    showGlslVal xs = "ivec2[](" ++ intercalate ", " (map showGlslVal xs) ++ ")"
    showGlslType = const "ivec2[]"
    glMap = fmap
    glZipWith = liftA2
    glZipWith3 = liftA3
    arrayLen = Prelude.length
    eltSize = const 4
    numComponents = const 2
    getGlslType = const OpenGL.Int
    uniformSet ul xs = let xs' = map toEnum $ concatMap toList xs in
        withArray xs' $ RawGL.glUniform2iv ul (fromIntegral $ Prelude.length xs')
instance GLType [Vec 3 Int] where
    showGlslVal xs = "ivec3[](" ++ intercalate ", " (map showGlslVal xs) ++ ")"
    showGlslType = const "ivec3[]"
    glMap = fmap
    glZipWith = liftA2
    glZipWith3 = liftA3
    arrayLen = Prelude.length
    eltSize = const 4
    numComponents = const 3
    getGlslType = const OpenGL.Int
    uniformSet ul xs = let xs' = map toEnum $ concatMap toList xs in
        withArray xs' $ RawGL.glUniform3iv ul (fromIntegral $ Prelude.length xs')
instance GLType [Vec 4 Int] where
    showGlslVal xs = "ivec4[](" ++ intercalate ", " (map showGlslVal xs) ++ ")"
    showGlslType = const "ivec4[]"
    glMap = fmap
    glZipWith = liftA2
    glZipWith3 = liftA3
    arrayLen = Prelude.length
    eltSize = const 4
    numComponents = const 4
    getGlslType = const OpenGL.Int
    uniformSet ul xs = let xs' = map toEnum $ concatMap toList xs in
        withArray xs' $ RawGL.glUniform4iv ul (fromIntegral $ Prelude.length xs')
instance GLType [UInt] where
    showGlslVal xs = "uint[](" ++ intercalate ", " (map showGlslVal xs) ++ ")"
    showGlslType = const "uint[]"
    glMap = fmap
    glZipWith = liftA2
    glZipWith3 = liftA3
    arrayLen = Prelude.length
    eltSize = const 4
    numComponents = const 1
    getGlslType = const OpenGL.UnsignedInt
    uniformSet ul xs = 
        withArray xs $ RawGL.glUniform1uiv ul (fromIntegral $ Prelude.length xs)
instance GLType [Vec 2 UInt] where
    showGlslVal xs = "uvec2[](" ++ intercalate ", " (map showGlslVal xs) ++ ")"
    showGlslType = const "uvec2[]"
    glMap = fmap
    glZipWith = liftA2
    glZipWith3 = liftA3
    arrayLen = Prelude.length
    eltSize = const 4
    numComponents = const 2
    getGlslType = const OpenGL.UnsignedInt
    uniformSet ul xs = let xs' = concatMap toList xs in
        withArray xs' $ RawGL.glUniform2uiv ul (fromIntegral $ Prelude.length xs')
instance GLType [Vec 3 UInt] where
    showGlslVal xs = "uvec3[](" ++ intercalate ", " (map showGlslVal xs) ++ ")"
    showGlslType = const "uvec3[]"
    glMap = fmap
    glZipWith = liftA2
    glZipWith3 = liftA3
    arrayLen = Prelude.length
    eltSize = const 4
    numComponents = const 3
    getGlslType = const OpenGL.UnsignedInt
    uniformSet ul xs = let xs' = concatMap toList xs in
        withArray xs' $ RawGL.glUniform3uiv ul (fromIntegral $ Prelude.length xs')
instance GLType [Vec 4 UInt] where
    showGlslVal xs = "uvec4[](" ++ intercalate ", " (map showGlslVal xs) ++ ")"
    showGlslType = const "uvec4[]"
    glMap = fmap
    glZipWith = liftA2
    glZipWith3 = liftA3
    arrayLen = Prelude.length
    eltSize = const 4
    numComponents = const 4
    getGlslType = const OpenGL.UnsignedInt
    uniformSet ul xs = let xs' = concatMap toList xs in
        withArray xs' $ RawGL.glUniform4uiv ul (fromIntegral $ Prelude.length xs')
instance GLType [Bool] where
    showGlslVal xs = "bool[](" ++ intercalate ", " (map showGlslVal xs) ++ ")"
    showGlslType = const "bool[]"
    glMap = fmap
    glZipWith = liftA2
    glZipWith3 = liftA3
    arrayLen = Prelude.length
    eltSize = const 1
    numComponents = const 1
    getGlslType = const OpenGL.Byte
    uniformSet ul xs = let xs' = map fromBool xs in
        withArray xs' $ RawGL.glUniform1iv ul (fromIntegral $ Prelude.length xs')
instance GLType [Vec 2 Bool] where
    showGlslVal xs = "bvec2[](" ++ intercalate ", " (map showGlslVal xs) ++ ")"
    showGlslType = const "bvec2[]"
    glMap = fmap
    glZipWith = liftA2
    glZipWith3 = liftA3
    arrayLen = Prelude.length
    eltSize = const 1
    numComponents = const 2
    getGlslType = const OpenGL.Byte
    uniformSet ul xs = let xs' = map fromBool $ concatMap toList xs in
        withArray xs' $ RawGL.glUniform2iv ul (fromIntegral $ Prelude.length xs')
instance GLType [Vec 3 Bool] where
    showGlslVal xs = "bvec3[](" ++ intercalate ", " (map showGlslVal xs) ++ ")"
    showGlslType = const "bvec3[]"
    glMap = fmap
    glZipWith = liftA2
    glZipWith3 = liftA3
    arrayLen = Prelude.length
    eltSize = const 1
    numComponents = const 3
    getGlslType = const OpenGL.Byte
    uniformSet ul xs = let xs' = map fromBool $ concatMap toList xs in
        withArray xs' $ RawGL.glUniform3iv ul (fromIntegral $ Prelude.length xs')
instance GLType [Vec 4 Bool] where
    showGlslVal xs = "bvec4[](" ++ intercalate ", " (map showGlslVal xs) ++ ")"
    showGlslType = const "bvec4[]"
    glMap = fmap
    glZipWith = liftA2
    glZipWith3 = liftA3
    arrayLen = Prelude.length
    eltSize = const 1
    numComponents = const 4
    getGlslType = const OpenGL.Byte
    uniformSet ul xs = let xs' = map fromBool $ concatMap toList xs in
        withArray xs' $ RawGL.glUniform4iv ul (fromIntegral $ Prelude.length xs')

fromBool = toEnum . fromEnum

printVec name xs = name ++ "(" ++ intercalate ", " (map showGlslVal xs) ++ ")"

makeMatSetter rawSetter ul xs = do
    m :: OpenGL.GLmatrix t <- OpenGL.newMatrix OpenGL.RowMajor xs
    OpenGL.withMatrix m $ const $ rawSetter ul 1 0


class GLType t => GLPrimOrVec t

instance GLPrimOrVec Float
instance GLPrimOrVec Double
instance GLPrimOrVec Int
instance GLPrimOrVec UInt
instance GLPrimOrVec (Vec 2 Float)
instance GLPrimOrVec (Vec 3 Float)
instance GLPrimOrVec (Vec 4 Float)
instance GLPrimOrVec (Vec 2 Double)
instance GLPrimOrVec (Vec 3 Double)
instance GLPrimOrVec (Vec 4 Double)
instance GLPrimOrVec (Vec 2 Int)
instance GLPrimOrVec (Vec 3 Int)
instance GLPrimOrVec (Vec 4 Int)
instance GLPrimOrVec (Vec 2 UInt)
instance GLPrimOrVec (Vec 3 UInt)
instance GLPrimOrVec (Vec 4 UInt)

class (GLPrimOrVec t, Storable (GLElt t)) => GLInputType t where
    toStorableList :: [t] -> [GLElt t]

instance GLInputType Float where
    toStorableList = id
instance GLInputType Double where
    toStorableList = id
instance GLInputType Int where
    toStorableList = id
instance GLInputType UInt where
    toStorableList = id
instance GLInputType (Vec 2 Float) where
    toStorableList = concatMap toList
instance GLInputType (Vec 3 Float) where
    toStorableList = concatMap toList
instance GLInputType (Vec 4 Float) where
    toStorableList = concatMap toList
instance GLInputType (Vec 2 Double) where
    toStorableList = concatMap toList
instance GLInputType (Vec 3 Double) where
    toStorableList = concatMap toList
instance GLInputType (Vec 4 Double) where
    toStorableList = concatMap toList
instance GLInputType (Vec 2 Int) where
    toStorableList = concatMap toList
instance GLInputType (Vec 3 Int) where
    toStorableList = concatMap toList
instance GLInputType (Vec 4 Int) where
    toStorableList = concatMap toList
instance GLInputType (Vec 2 UInt) where
    toStorableList = concatMap toList
instance GLInputType (Vec 3 UInt) where
    toStorableList = concatMap toList
instance GLInputType (Vec 4 UInt) where
    toStorableList = concatMap toList

class GLInputType t => GLSupportsSmoothInterp t

instance GLSupportsSmoothInterp Float
instance GLSupportsSmoothInterp (Vec 2 Float)
instance GLSupportsSmoothInterp (Vec 3 Float)
instance GLSupportsSmoothInterp (Vec 4 Float)

class (GLType t, Integral (GLElt t), Bits (GLElt t)) => GLSupportsBitwiseOps t

instance GLSupportsBitwiseOps Int
instance GLSupportsBitwiseOps UInt
instance GLSupportsBitwiseOps (Vec 2 Int)
instance GLSupportsBitwiseOps (Vec 3 Int)
instance GLSupportsBitwiseOps (Vec 4 Int)
instance GLSupportsBitwiseOps (Vec 2 UInt)
instance GLSupportsBitwiseOps (Vec 3 UInt)
instance GLSupportsBitwiseOps (Vec 4 UInt)


type family GLElt t where
    GLElt (Mat r c t) = t
    GLElt [t] = t
    GLElt Float = Float
    GLElt Double = Double
    GLElt Int = Int
    GLElt UInt = UInt
    GLElt Bool = Bool


-- * Primitive GLTypes

class (GLType t, Storable t, Enum t, Eq t, Ord t) => GLPrim t where
    cast :: GLPrim t0 => t0 -> t
instance GLPrim Float where
    cast = fromIntegral . fromEnum
instance GLPrim Double where
    cast = fromIntegral . fromEnum
instance GLPrim Int where
    cast = fromEnum
instance GLPrim UInt where
    cast = fromIntegral . fromEnum
instance GLPrim Bool where
    cast = (/= toEnum 0)

class (GLPrim t, Storable t, Enum t, Eq t, Ord t) => GLSingle t
instance GLSingle Float
instance GLSingle Int
instance GLSingle UInt
instance GLSingle Bool

class (GLPrim t, Num t) => GLNumeric t where
    genDiv :: t -> t -> t
instance GLNumeric Float where genDiv = (/)
instance GLNumeric Double where genDiv = (/)
instance GLNumeric Int where genDiv = div
instance GLNumeric UInt where genDiv = div

class GLNumeric t => GLSigned t where
instance GLSigned Float
instance GLSigned Double
instance GLSigned Int

class (GLSigned t, RealFrac t, Floating t) => GLFloating t
instance GLFloating Float
instance GLFloating Double

class GLSigned t => GLSingleNumeric t
instance GLSingleNumeric Float
instance GLSingleNumeric Int

class (GLPrim t, Integral t, Bits t) => GLInteger t
instance GLInteger Int
instance GLInteger UInt
