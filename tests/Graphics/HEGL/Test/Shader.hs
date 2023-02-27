module Graphics.HEGL.Test.Shader where

import Test.HUnit
import Graphics.HEGL

type ShaderTest = FragExpr Bool

-- verify that the test produces a white image
shaderTestCase :: String -> ShaderTest -> Test
shaderTestCase = undefined

shaderTests :: [Test]
shaderTests = []

