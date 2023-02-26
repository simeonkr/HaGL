module Graphics.HEGL.CodeGen (
    GLProgram(..),
    UniformVar(..), InpVar(..),
    genProgram
) where

import Control.Monad.State.Lazy (State, execState, runState, get, gets, modify)
import Data.Set as Set
import Foreign.Storable

import Graphics.HEGL.ExprID
import Graphics.HEGL.GLType
import Graphics.HEGL.GLExpr
import Graphics.HEGL.GLObj
import Graphics.HEGL.Shader


-- GLProgram = output of code gen for a GLObj

data GLProgram = GLProgram {
    primitiveMode :: PrimitiveMode,
    indices :: Maybe [ConstExpr UInt],
    uniformVars :: Set.Set UniformVar,
    inputVars :: Set.Set InpVar,
    vertexShader :: Shader,
    fragmentShader :: Shader
}

data UniformVar where
    UniformVar :: GLType t => ExprID -> GLExpr HostDomain t -> UniformVar

data InpVar where
    InpVar :: (GLInputType t, GLType (GLElt t), Storable (GLElt t)) => 
        ExprID -> [GLExpr ConstDomain t] -> InpVar


genProgram :: GLObj -> GLProgram
genProgram = undefined
