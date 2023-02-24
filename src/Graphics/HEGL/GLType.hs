module Graphics.HEGL.GLType (
    UInt,
    GLType(..),
    GLElt,
    GLPrim, GLSingle, GLNumeric, GLFloating, GLSingleNumeric, GLInteger,
    genDiv
) where

import Data.Bits
import Data.Word (Word32)
import Foreign.Storable (Storable)
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
    toStorableList :: [t] -> [GLElt t]
    eltSize :: [t] -> Int
    numComponents :: [t] -> Int
    getGlslType :: [t] -> OpenGL.DataType
    uniformSet :: OpenGL.GLint -> t -> IO ()

instance GLType Float where
    showGlslType = const "float"
instance GLType Double
instance GLType Int
instance GLType UInt
instance GLType Bool
instance GLType (Vec 2 Float)
instance GLType (Vec 3 Float)
instance GLType (Vec 4 Float)
instance GLType (Vec 2 Double)
instance GLType (Vec 3 Double)
instance GLType (Vec 4 Double)
instance GLType (Vec 2 Int)
instance GLType (Vec 3 Int)
instance GLType (Vec 4 Int)
instance GLType (Vec 2 UInt)
instance GLType (Vec 3 UInt)
instance GLType (Vec 4 UInt)
instance GLType (Vec 2 Bool)
instance GLType (Vec 3 Bool)
instance GLType (Vec 4 Bool)
instance GLType (Mat 2 2 Float)
instance GLType (Mat 2 3 Float)
instance GLType (Mat 2 4 Float)
instance GLType (Mat 3 2 Float)
instance GLType (Mat 3 3 Float)
instance GLType (Mat 3 4 Float)
instance GLType (Mat 4 2 Float)
instance GLType (Mat 4 3 Float)
instance GLType (Mat 4 4 Float)
instance GLType (Mat 2 2 Double)
instance GLType (Mat 2 3 Double)
instance GLType (Mat 2 4 Double)
instance GLType (Mat 3 2 Double)
instance GLType (Mat 3 3 Double)
instance GLType (Mat 3 4 Double)
instance GLType (Mat 4 2 Double)
instance GLType (Mat 4 3 Double)
instance GLType (Mat 4 4 Double)
instance GLType [Float]
instance GLType [Double]
instance GLType [Int]
instance GLType [UInt]
instance GLType [Bool]


type family GLElt t where
    GLElt (Mat r c t) = t
    GLElt [t] = t
    GLElt Float = Float
    GLElt Double = Double
    GLElt Int = Int
    GLElt UInt = UInt
    GLElt Bool = Bool


-- * "Subclasses" of GLType

class (GLType t, Storable t, Enum t, Eq t, Ord t) => GLPrim t
instance GLPrim Float
instance GLPrim Double
instance GLPrim Int
instance GLPrim UInt
instance GLPrim Bool

class (GLPrim t, Storable t, Enum t, Eq t, Ord t) => GLSingle t
instance GLSingle Float
instance GLSingle Int
instance GLSingle UInt
instance GLSingle Bool

class (GLPrim t, Storable t, Num t, Enum t, Eq t, Ord t) => GLNumeric t where
    genDiv :: t -> t -> t
instance GLNumeric Float where genDiv = (/)
instance GLNumeric Double where genDiv = (/)
instance GLNumeric Int where genDiv = div
instance GLNumeric UInt where genDiv = div

class (GLNumeric t, Storable t, Num t, Enum t, Eq t, Ord t, RealFrac t, Floating t) => GLFloating t
instance GLFloating Float
instance GLFloating Double

class (GLNumeric t, Storable t, Num t, Enum t, Eq t, Ord t) => GLSingleNumeric t
instance GLSingleNumeric Float
instance GLSingleNumeric Int
instance GLSingleNumeric UInt

class (GLSingleNumeric t, Storable t, Num t, Enum t, Eq t, Ord t, Integral t, Bits t) => GLInteger t
instance GLInteger Int
instance GLInteger UInt
