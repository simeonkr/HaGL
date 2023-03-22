module Graphics.HEGL.Test.Shader where

import Test.HUnit
import Control.Monad (when)
import System.Directory (removeFile)
import qualified Data.ByteString as BS

import Graphics.HEGL
import Graphics.HEGL.Internal (dumpGlsl)


type ShaderTest = FragExpr Bool

-- verify that the test produces a white image
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
    -- as it generates shader code a second tim
    let captureFile = "dist/test/" ++ label ++ ".ppm"
    drawGlutCustom (defaultGlutOptions { 
        runMode = GlutCaptureAndExit captureFile }) obj
    dat <- BS.readFile captureFile
    let success = BS.all (== 0xff) . BS.drop (BS.length dat - 16) $ dat
    when success $ removeFile captureFile
    assertBool descr success


shaderTests :: [Test]
shaderTests = [
    shaderTestCase "shader_trivial" "trivially white background" trivial,
    shaderTestCase "shader_trivial2" "trivially white background" trivial2
    ]

trivial = true

trivial2 = let one = 1 :: FragExpr Float in one .== one
