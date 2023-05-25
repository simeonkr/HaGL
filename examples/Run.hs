import Data.List (find)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import Graphics.HaGL
import Graphics.HaGL.Examples


runExample :: String -> [GLObj] -> IO ()
runExample label objs = do
    createDirectoryIfMissing True "output"
    let captureFile = "output/" ++ label ++ ".ppm"
    drawGlutCustom (defaultGlutOptions { 
        glClearColor = (1, 1, 1, 1),
        runMode = GlutCaptureLatest captureFile }) objs

main = do
    args <- getArgs
    case args of
        [] -> fail "Please provide an example name as an argument"
        (exampleName:_) -> case find ((== exampleName) . fst) exampleList of
            Just (_, example) -> runExample exampleName example
            Nothing -> fail "Example not found"
