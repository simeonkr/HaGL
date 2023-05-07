module Graphics.HEGL.Internal (
    constEval,
    hostEval,
    dumpGlsl,
    genericUniform,
    GLExprException(..),
    GLObjException(..),
    EvalException(..)
) where

import Graphics.HEGL.GLType (GLType)
import Graphics.HEGL.ExprID (genID)
import Graphics.HEGL.GLExpr
import Graphics.HEGL.GLObj
import Graphics.HEGL.Eval
import Graphics.HEGL.CodeGen
import Graphics.HEGL.Print

dumpGlsl :: GLObj -> String
dumpGlsl = show . genProgram

genericUniform :: GLType t => String -> GLExpr d t
genericUniform label = GLAtom (genID ()) $ GenericUniform label
