module Graphics.HEGL.Backend.GLUT (
    GlutOptions(..),
    runGlut
) where

import Prelude hiding (id)
import Control.Monad (when)
import Data.Functor.Identity
import Data.IORef
import Data.Time.Clock
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT 

import Graphics.HEGL.Backend
import Graphics.HEGL.GLType
import Graphics.HEGL.GLExpr
import Graphics.HEGL.ExprID
import Graphics.HEGL.GLObj (GLObj)
import Graphics.HEGL.Eval
import Graphics.HEGL.CodeGen (UniformVar(..))

import qualified Graphics.HEGL.Util.DepMap as DepMap


data GlutOptions = GlutOptions {
    winPosition :: Maybe (GLint, GLint),
    winSize :: (GLsizei, GLsizei),
    winFullscreen :: Bool,
    winTitle :: Maybe String,
    glLineWidth :: GLfloat
}


runGlut :: GlutOptions -> [GLObj] -> IO ()
runGlut options glObjs = do
    initWindow options

    ioState <- initIOState  
    
    runObjs <- mapM genRunObj glObjs
    
    idleCallback $= Just (update ioState runObjs)
    displayCallback $= display runObjs
    mouseCallback $= Just (mouse ioState)
    motionCallback $= Just (motion ioState)
    passiveMotionCallback $= Just (motion ioState)

    lineWidth $= glLineWidth options

    mainLoop

initWindow :: GlutOptions -> IO ()
initWindow options = do
    (progName, args) <- getArgsAndInitialize
    _ <- createWindow progName
    maybe (return ()) (\(x, y) -> windowPosition $= Position x y) 
        (winPosition options)
    windowSize $= (\(x, y) -> Size x y) (winSize options)
    when (winFullscreen options) fullScreen
    maybe (return ()) (windowTitle $=) (winTitle options) 
    initialDisplayMode $= [RGBAMode, WithAlphaComponent]
    depthFunc $= Just Lequal


-- I/O state

data IOState = IOState {
    initTime :: Float,
    mouseLeftDown :: Bool,
    mouseRightDown :: Bool,
    mouseWheel :: Float,
    curMouseX :: Int,
    curMouseY :: Int,
    -- for stats such as FPS
    totUpdates :: Int,
    lastStatsUpdate :: Float
}

defIOState :: IOState
defIOState = IOState {
    initTime = 0,
    mouseLeftDown = False,
    mouseRightDown = False,
    mouseWheel = 0,
    curMouseX = 0,
    curMouseY = 0,
    totUpdates = 0,
    lastStatsUpdate = 0
}

initIOState :: IO (IORef IOState)
initIOState = do
    epoch <- getCurrentTime
    let initTime = fromRational $ toRational $ utctDayTime epoch
    newIORef defIOState { initTime = initTime } 


-- Update logic

update :: IORef IOState -> [RunObj] -> IdleCallback
update ioState objs = do
    printStats ioState
    ioStateUpdate ioState
    ioState <- readIORef ioState
    let updateObj obj = do
            currentProgram $= Just (prog obj)
            mapM_ (setUniform ioState obj) (uniformVars obj)
            -- prepare precMap for the next iteration
            updatePrecMap ioState obj
    mapM_ updateObj objs
    postRedisplay Nothing

printStats :: IORef IOState -> IO ()
printStats ioStateRef = do
    ioState <- readIORef ioStateRef
    let numUpdates = totUpdates ioState
    epoch <- getCurrentTime
    let t = fromRational $ toRational $ utctDayTime epoch
        dt = t - lastStatsUpdate ioState
    if dt > 1 then do
        writeIORef ioStateRef $ ioState { 
            lastStatsUpdate = t, totUpdates = 0 }
        putStrLn $ "FPS: " ++ show (floor $ fromIntegral numUpdates / dt :: Int)
    else writeIORef ioStateRef $ ioState { totUpdates = numUpdates + 1 }

setUniform :: IOState -> RunObj -> UniformVar -> IO ()
setUniform ioState obj (UniformVar id x) = do
    (UniformLocation ul) <- get (uniformLocation (prog obj) (idLabel id))
    val <- hostEval (ioEval ioState obj) x
    uniformSet ul val

updatePrecMap :: IOState -> RunObj -> IO ()
updatePrecMap ioState obj = do
    let updateVal ioState obj expr _ =
            Identity <$> hostEval (ioEval ioState obj) expr
    pm <- readIORef $ precMap obj
    pm1 <- DepMap.traverseWithKey (updateVal ioState obj) pm
    writeIORef (precMap obj) pm1


-- Draw logic

display :: [RunObj] -> DisplayCallback
display objs = do
    clear [ColorBuffer, DepthBuffer]
    let doVao obj = do
            currentProgram $= Just (prog obj)
            bindVertexArrayObject $= Just (vao obj)
            draw (primitiveMode obj) (indices obj) (numVerts obj)
    mapM_ doVao objs
    flush

draw :: PrimitiveMode -> Maybe [ConstExpr UInt] -> Int -> IO ()
draw mode Nothing n = 
    drawArrays mode 0 (fromIntegral n)
draw mode (Just inds) _ =
    drawElements mode (fromIntegral $ length inds) UnsignedInt (makeOff 0)


-- I/O

ioStateUpdate :: IORef IOState -> IO ()
ioStateUpdate ioState =
    let decayWheel ioState = ioState { mouseWheel = 0.1 * mouseWheel ioState }
    in modifyIORef ioState decayWheel

mouse :: IORef IOState -> MouseCallback
mouse ioState LeftButton mouseState _ =
    let updateLeft ioState = ioState { mouseLeftDown = mouseState == Down }
    in modifyIORef ioState updateLeft
mouse ioState RightButton mouseState _ = 
    let updateRight ioState = ioState { mouseRightDown = mouseState == Down }
    in modifyIORef ioState updateRight
mouse ioState WheelUp _ _ =
    let updateWheel ioState = ioState { mouseWheel = 1 }
    in modifyIORef ioState updateWheel
mouse ioState WheelDown _ _ =
    let updateWheel ioState = ioState { mouseWheel = -1 }
    in modifyIORef ioState updateWheel
mouse _ _ _ _ = return ()

motion :: IORef IOState -> MotionCallback
motion ioState (Position x y) =
    let updatePos ioState = ioState { curMouseX = fromIntegral x, curMouseY = fromIntegral y }
    in modifyIORef ioState updatePos

ioEval :: IOState -> RunObj -> GLExpr HostDomain t -> IO t

ioEval ioState obj (GLAtom _ (IOPrec x0 x)) = do
    pm <- readIORef $ precMap obj
    case DepMap.lookup x pm of
        -- if this strategy causes problems, store a variable in ioState indicating whether
        -- we've initialized the ioPrecs or not and use it to determine what value to return
        Just val -> return $ runIdentity val
        Nothing -> do
            _ <- ioEval ioState obj x
            val <- ioEval ioState obj x0
            writeIORef (precMap obj) $ DepMap.insert x0 (Identity val) pm
            return val

ioEval ioState _ (GLAtom _ (IOFloat "time")) = do
    let t0 = initTime ioState
    epoch <- getCurrentTime
    let t = fromRational $ toRational $ utctDayTime epoch
    return $ t - t0

ioEval ioState _ (GLAtom _ (IOBool "mouseLeft")) =
    return $ (toEnum . fromEnum) $ mouseLeftDown ioState

ioEval ioState _ (GLAtom _ (IOBool "mouseRight")) =
    return $ (toEnum . fromEnum) $ mouseRightDown ioState

ioEval ioState _ (GLAtom _ (IOFloat "mouseWheel")) =
    return $ mouseWheel ioState

ioEval ioState _ (GLAtom _ (IOFloat "mouseX")) = do
    (Size width _) <- get windowSize
    return $ fromIntegral (curMouseX ioState) / fromIntegral width

ioEval ioState _ (GLAtom _ (IOFloat "mouseY")) = do
    (Size _ height) <- get windowSize
    return $ 1 - fromIntegral (curMouseY ioState) / fromIntegral height

