module Graphics.HEGL.GLObj (
    GLObj(..),
    PrimitiveMode(..),
    GLObjException(..)
) where

import Control.Exception (Exception)
import Graphics.Rendering.OpenGL.GL.PrimitiveMode (PrimitiveMode(..))

import Graphics.HEGL.Numerical (Vec)
import Graphics.HEGL.GLType
import Graphics.HEGL.GLExpr


data GLObj = GLObj {
    primitiveMode :: PrimitiveMode,
    indices :: Maybe [ConstExpr UInt],
    position :: VertExpr (Vec 4 Float),
    color :: FragExpr (Vec 4 Float),
    discardWhen :: FragExpr Bool
}


data GLObjException =
    NoInputVars |
    EmptyInputVar |
    MismatchedInputVars
    deriving Eq

instance Show GLObjException where
    show NoInputVars = "Attempted to process a GLObj containing no input variables"
    show EmptyInputVar = "Input variable initialized using empty list"
    show MismatchedInputVars = "Dimensions of input variables do not match"

instance Exception GLObjException
