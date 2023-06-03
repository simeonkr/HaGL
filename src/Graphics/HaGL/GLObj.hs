module Graphics.HaGL.GLObj (
    GLObj(..),
    Graphics.HaGL.GLObj.PrimitiveMode(..),
    GLObjException(..)
) where

import Control.Exception (Exception)
import qualified Graphics.Rendering.OpenGL.GL.PrimitiveMode (PrimitiveMode(..))

import Graphics.HaGL.Numerical (Vec)
import Graphics.HaGL.GLType
import Graphics.HaGL.GLExpr


-- TODO: should the indices be bundled with the PrimitiveMode?
-- | A drawable object specified by a set of variables of type 'GLExpr' and
-- the 'PrimitiveMode' according to which output vertices of the variable 
-- 'position', indexed by 'indices', should be interpreted. 
-- When using the convenience functions 'points', 'triangles', etc., to define a 
--'GLObj' with the corresponding 'PrimitiveMode', at the very minimum the fields 
-- 'position' and 'color' must be set before drawing the 'GLObj'.
data GLObj = GLObj {
    -- | The 'PrimitiveMode' that will be used to draw the object
    primitiveMode :: PrimitiveMode,
    -- | A set of position indices used to construct the primitives of the object
    indices :: Maybe [ConstExpr UInt],
    -- | A vertex variable specifying the position of an arbitrary vertex
    position :: VertExpr (Vec 4 Float),
    -- | A fragment variable specifying the color of an arbitrary fragment
    color :: FragExpr (Vec 4 Float),
    -- | An fragment variable specifying the condition for discarding an arbitrary
    -- fragment
    discardWhen :: FragExpr Bool
}

-- | See [Graphics.Rendering.OpenGL.GL.PrimitiveMode]
-- (https://hackage.haskell.org/package/OpenGL-3.0.3.0/docs/Graphics-Rendering-OpenGL-GL-PrimitiveMode.html#g:1)
-- for a description of each @PrimitiveMode@
type PrimitiveMode = 
    Graphics.Rendering.OpenGL.GL.PrimitiveMode.PrimitiveMode

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
