module Graphics.HEGL.GLObj (
    GLObj(..),
    PrimitiveMode(..)
) where

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
