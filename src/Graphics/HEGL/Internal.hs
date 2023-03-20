module Graphics.HEGL.Internal (
    constEval,
    hostEval,
    dumpGlsl
) where

import Graphics.HEGL.GLObj
import Graphics.HEGL.Eval
import Graphics.HEGL.CodeGen
import Graphics.HEGL.Shader
import Graphics.HEGL.Print

dumpGlsl :: GLObj -> String
dumpGlsl = show . genProgram
