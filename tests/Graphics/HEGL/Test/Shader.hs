module Graphics.HEGL.Test.Shader where

import Test.HUnit
import Graphics.HEGL
import Graphics.HEGL.Internal (dumpGlsl)

type ShaderTest = FragExpr Bool

-- verify that the test produces a white image
-- temporary solution: manually verify white background
shaderTestCase :: String -> String -> ShaderTest -> Test
shaderTestCase label descr test = TestLabel label $ TestCase $ do
    let quadPos = vert 
            [(vec2 (-1) (-1)), 
            (vec2 (-1) 1), 
            (vec2 1 (-1)), 
            (vec2 1 1)]
        vPos = quadPos $- vec2 0 1
        color = cast test .* 1
        obj = triangleStrip { position = vPos, color = color } 
        astDump = show test
        glsl = dumpGlsl obj
    writeFile ("dist/test/" ++ label ++ ".dump") astDump
    writeFile ("dist/test/" ++ label ++ ".glsl") glsl
    -- TODO: calling drawGlut directly is inefficient 
    -- as it generates shader code a second time
    drawGlut obj

shaderTests :: [Test]
shaderTests = [
    shaderTestCase "shader_trivial" "trivially white background" trivial
    ]

trivial = Graphics.HEGL.const $ toEnum 1
