import Control.Monad (when)
import Data.List (find)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import Graphics.Rendering.OpenGL (($=), clearColor, Color4(..))
import Graphics.HaGL
import Graphics.HaGL.Examples


main = do
    args <- getArgs
    case args of
        [exampleName] ->
            runExample GlutNormal exampleName
        [exampleName, "-s"] -> do
            let captureFile = "output/" ++ exampleName
            createDirectoryIfMissing True "output"
            runExample (GlutCaptureLatest captureFile) exampleName
        [exampleName, "-a"] -> do
            let captureFile = "output/" ++ exampleName
            createDirectoryIfMissing True "output"
            createDirectoryIfMissing True $ "output/" ++ exampleName
            runExample (GlutCaptureFrames captureFile) exampleName 
        _ -> fail $ "Please provide an example name as an argument, " ++
            "followed by an optional flag -s to capture a single frame, " ++
            "or -a to capture all frames"
    where
        runExample runMode exampleName = 
            case find ((== exampleName) . fst) exampleList of
                Just (_, example) ->
                    drawGlutCustom (defaultGlutOptions { 
                        openGLSetup = do {
                            clearColor $= Color4 1 1 1 1;
                        },
                        runMode = runMode }) example
                Nothing -> fail "Example not found"
