module Graphics.HaGL.Internal (
    constEval,
    hostEval,
    dumpGlsl,
    genericUniform,
    GLExprException(..),
    GLObjException(..),
    EvalException(..)
) where

import Graphics.HaGL.GLType (GLType)
import Graphics.HaGL.ExprID (genID)
import Graphics.HaGL.GLExpr
import Graphics.HaGL.GLObj
import Graphics.HaGL.Eval
import Graphics.HaGL.CodeGen
import Graphics.HaGL.Print

dumpGlsl :: GLObj -> String
dumpGlsl = show . genProgram

genericUniform :: GLType t => String -> GLExpr d t
genericUniform label = GLAtom (genID ()) $ GenericUniform label
