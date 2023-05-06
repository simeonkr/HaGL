module Graphics.HEGL.Internal (
    constEval,
    hostEval,
    dumpGlsl,
    GLExprException(..)
) where

import Graphics.HEGL.GLExpr
import Graphics.HEGL.GLObj
import Graphics.HEGL.Eval
import Graphics.HEGL.CodeGen
import Graphics.HEGL.Print

dumpGlsl :: GLObj -> String
dumpGlsl = show . genProgram
