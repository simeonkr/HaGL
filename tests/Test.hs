import Test.HUnit
import System.Exit
import Graphics.HEGL.Test.Host
import Graphics.HEGL.Test.Shader


runTests :: Test -> IO ()
runTests tests = do
    count <- runTestTT tests
    if errors count > 0 || failures count > 0 then exitFailure else return ()

main :: IO ()
main = do
    runTests $ TestList hostTests
    runTests $ TestList shaderTests
