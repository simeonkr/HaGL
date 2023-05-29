{-# LANGUAGE ScopedTypeVariables #-}

module Graphics.HaGL.Backend.GLUT (
    GlutOptions(..),
    GlutRunMode(..),
    runGlut
) where

import Prelude hiding (id)
import Control.Monad (when)
import Data.Functor.Identity
import Data.IORef
import Data.Time.Clock
import qualified Data.ByteString as BS
import Graphics.Rendering.OpenGL
import Graphics.Rendering.OpenGL.Capture
import Graphics.UI.GLUT 

import Graphics.HaGL.Backend
import Graphics.HaGL.GLType
import Graphics.HaGL.GLExpr
import Graphics.HaGL.ExprID
import Graphics.HaGL.GLObj (GLObj)
import Graphics.HaGL.Eval
import Graphics.HaGL.CodeGen (UniformVar(..))

import qualified Graphics.HaGL.Util.DepMap as DepMap


data GlutOptions = GlutOptions {
    winPosition :: Maybe (GLint, GLint),
    winSize :: (GLsizei, GLsizei),
    winFullscreen :: Bool,
    winTitle :: Maybe String,
    glClearColor :: (GLfloat, GLfloat, GLfloat, GLfloat),
    glLineWidth :: GLfloat,
    runMode :: GlutRunMode
}

data GlutRunMode =
    GlutNormal |
    GlutCaptureLatest String |
    GlutCaptureFrames String |
    GlutCaptureAndExit String


runGlut :: GlutOptions -> [GLObj] -> IO ()
runGlut options glObjs = do
    initWindow options

    ioState <- initIOState  
    
    runObjs <- mapM genRunObj glObjs
    
    idleCallback $= Just (update (runMode options) ioState runObjs)
    displayCallback $= display runObjs
    mouseCallback $= Just (mouse ioState)
    motionCallback $= Just (motion ioState)
    passiveMotionCallback $= Just (motion ioState)

    let (r, g, b, a) = glClearColor options
    clearColor $= Color4 r g b a
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
    actionOnWindowClose $= MainLoopReturns 
    -- TODO: make settings like these customizable
    initialDisplayMode $= [RGBAMode, WithAlphaComponent]
    depthFunc $= Just Lequal
    blend $= Enabled
    blendEquation $= FuncAdd
    blendFunc $= (SrcAlpha, OneMinusSrcAlpha)

-- I/O state

data IOState = IOState {
    initTime :: Float,
    precMap :: IORef (DepMap.DepMap (GLExpr HostDomain) Identity),
    mouseLeftDown :: Bool,
    mouseRightDown :: Bool,
    mouseWheel :: Float,
    curMouseX :: Int,
    curMouseY :: Int,
    -- for stats such as FPS
    totUpdates :: Int,
    curNumUpdates :: Int,
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
    curNumUpdates = 0,
    lastStatsUpdate = 0
}

initIOState :: IO (IORef IOState)
initIOState = do
    epoch <- getCurrentTime
    let initTime = fromRational $ toRational $ utctDayTime epoch
    pm <- newIORef DepMap.empty
    newIORef defIOState { initTime = initTime, lastStatsUpdate = initTime, precMap = pm  }


-- Update logic

update :: GlutRunMode -> IORef IOState -> [RunObj] -> IdleCallback
update runMode ioState objs = do
    outputStatsAndCapture runMode ioState
    ioStateUpdate ioState
    ioState <- readIORef ioState
    let updateObj obj = do
            currentProgram $= Just (prog obj)
            mapM_ (setUniform ioState obj) (uniformVars obj)
    mapM_ updateObj objs
    -- prepare precMap for the next iteration
    updatePrecMap ioState
    postRedisplay Nothing

outputStatsAndCapture :: GlutRunMode -> IORef IOState -> IO ()
outputStatsAndCapture runMode ioStateRef = do
    ioState <- readIORef ioStateRef
    let numUpdates = curNumUpdates ioState
    epoch <- getCurrentTime
    let t = fromRational $ toRational $ utctDayTime epoch
        dt = t - lastStatsUpdate ioState
    if dt > 1 
        then do
            writeIORef ioStateRef $ ioState { 
                totUpdates = totUpdates ioState + 1, curNumUpdates = 0, lastStatsUpdate = t }
            putStrLn $ "FPS: " ++ show (floor $ fromIntegral numUpdates / dt :: Int)
        else 
            writeIORef ioStateRef $ ioState { 
                totUpdates = totUpdates ioState + 1, curNumUpdates = numUpdates + 1 }
    -- TODO: implement own capturePPM/PNG to remove the unecessary dependency
    let captureToFile fname = capturePPM >>= BS.writeFile (fname ++ ".ppm")
    case runMode of
        GlutNormal -> return ()
        GlutCaptureLatest fname -> 
            when (dt > 0.1) (captureToFile fname)
        GlutCaptureFrames fname ->
            captureToFile $ fname ++ "/frame" ++ show (totUpdates ioState)
        GlutCaptureAndExit fname ->
            when (totUpdates ioState > 30) $ do
                captureToFile fname
                leaveMainLoop

-- note that shared uniforms are evaluated separately for each object
-- (except for any prec subexpressions)
-- it's possible that this may cause unexpected behaviour, in
-- which case a map of pre-computed shared values will be needed
setUniform :: IOState -> RunObj -> UniformVar -> IO ()
setUniform ioState obj (UniformVar id x) = do
    (UniformLocation ul) <- get (uniformLocation (prog obj) (idLabel id))
    val <- hostEval (ioEval ioState) x
    uniformSet ul val

updatePrecMap :: IOState -> IO ()
updatePrecMap ioState = do
    let updateVal ioState (GLAtom _ (IOPrec _ x) :: GLExpr HostDomain t) _ =
            Identity <$> hostEval (ioEval ioState) x
    pm <- readIORef $ precMap ioState
    _ <- DepMap.traverseWithKey (updateVal ioState) pm
    -- pm might have new keys so we need to read it again
    -- FIXME: find an alternative to this really ugly solution
    pm <- readIORef $ precMap ioState
    pm1 <- DepMap.traverseWithKey (updateVal ioState) pm
    writeIORef (precMap ioState) pm1


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

ioEval :: IOState -> GLExpr HostDomain t -> IO t

ioEval ioState e@(GLAtom _ (IOPrec x0 x)) = do
    pm <- readIORef $ precMap ioState
    case DepMap.lookup e pm of
        Just val -> return $ runIdentity val
        Nothing -> do
            val <- hostEval (ioEval ioState) x0
            writeIORef (precMap ioState) $ DepMap.insert e (Identity val) pm
            return val

ioEval ioState (GLAtom _ (IOFloat "time")) = do
    let t0 = initTime ioState
    epoch <- getCurrentTime
    let t = fromRational $ toRational $ utctDayTime epoch
    return $ t - t0

ioEval ioState (GLAtom _ (IOBool "mouseLeft")) =
    return $ (toEnum . fromEnum) $ mouseLeftDown ioState

ioEval ioState (GLAtom _ (IOBool "mouseRight")) =
    return $ (toEnum . fromEnum) $ mouseRightDown ioState

ioEval ioState (GLAtom _ (IOFloat "mouseWheel")) =
    return $ mouseWheel ioState

ioEval ioState (GLAtom _ (IOFloat "mouseX")) = do
    (Size width _) <- get windowSize
    return $ fromIntegral (curMouseX ioState) / fromIntegral width

ioEval ioState (GLAtom _ (IOFloat "mouseY")) = do
    (Size _ height) <- get windowSize
    return $ 1 - fromIntegral (curMouseY ioState) / fromIntegral height

