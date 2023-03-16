module Graphics.HEGL.Test.Shader where

import Test.HUnit
import Graphics.HEGL

type ShaderTest = FragExpr Bool

-- verify that the test produces a white image
shaderTestCase :: String -> ShaderTest -> Test
shaderTestCase = undefined

-- temporary solution: manually verify white background
showShaderTest :: ShaderTest -> IO ()
showShaderTest test = drawGLUT $ 
    triangleStrip { position = vPos, color = color } where
        quadPos = vert 
            [(vec2 (-1) (-1)), 
            (vec2 (-1) 1), 
            (vec2 1 (-1)), 
            (vec2 1 1)]
        vPos = quadPos $- vec2 0 1
        pos = frag quadPos
        color = cast test .* 1

shaderTests :: [Test]
shaderTests = [
    shaderTestCase "trivially white background" trivial
    ]

trivial = Graphics.HEGL.const $ toEnum 1
