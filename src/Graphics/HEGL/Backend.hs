module Graphics.HEGL.Backend (
    RunObj(..),
    genRunObj,
    makeOff
) where

import Prelude hiding (id)
import Control.Exception (assert)
import Control.Monad (unless)
import Data.Functor.Identity
import Data.IORef
import Foreign.Marshal.Array (withArray)
import Foreign.Ptr (Ptr, wordPtrToPtr)
import Graphics.Rendering.OpenGL

import qualified Data.Set as Set

import Graphics.HEGL.GLType
import Graphics.HEGL.GLExpr
import Graphics.HEGL.ExprID
import Graphics.HEGL.GLObj
import Graphics.HEGL.Eval
import Graphics.HEGL.CodeGen (GLProgram(GLProgram), InpVar(..), UniformVar(..), genProgram)

import qualified Graphics.HEGL.Util.DepMap as DepMap


-- RunObj = GLProgram transformed to low-level OpenGL data

data RunObj = RunObj {
    primitiveMode :: PrimitiveMode,
    indices :: Maybe [ConstExpr UInt],
    uniformVars :: Set.Set UniformVar,
    precMap :: IORef (DepMap.DepMap (GLExpr HostDomain) Identity),
    numVerts :: Int,
    vao :: VertexArrayObject,
    prog :: Program
}

genRunObj :: GLObj -> IO RunObj
genRunObj = progToRunObj . genProgram

progToRunObj :: GLProgram -> IO RunObj
progToRunObj (GLProgram primitiveMode indices 
  uniformVars inputVars vertexShader fragmentShader) = do

    vs <- loadShader VertexShader $ show vertexShader
    fs <- loadShader FragmentShader $ show fragmentShader
    prog <- createProgram
    attachShader prog vs
    attachShader prog fs
    linkProgram prog

    vao <- genObjectName
    bindVertexArrayObject $= Just vao

    -- TODO: it is more efficient to form a
    -- single buffer from all the input data
    mapM_ (bindAttrDat prog) inputVars

    bindIndices indices

    precMap <- newIORef DepMap.empty

    return $ RunObj primitiveMode indices
        uniformVars precMap (getNumElts inputVars) vao prog

loadShader :: ShaderType -> String -> IO Shader
loadShader stype src = do
    shader <- createShader stype
    shaderSourceBS shader $= packUtf8 src
    compileShader shader
    ok <- get (compileStatus shader)
    infoLog <- get (shaderInfoLog shader)
    unless (null infoLog || infoLog == "\NUL")
        (mapM_ putStrLn ["Shader info log:", infoLog, ""])
    unless ok $ do
        deleteObjectName shader
        ioError (userError "shader compilation failed")
    return shader

bindAttrDat :: Program -> InpVar -> IO ()
bindAttrDat prog (InpVar id xs) = do
    arrayBuffer <- genObjectName
    bindBuffer ArrayBuffer $= Just arrayBuffer
    let val = map constEval xs
    let size = fromIntegral $ eltSize val * numComponents val * length val
    withArray (toStorableList val) $ \ptr ->
        bufferData ArrayBuffer $= (size, ptr, StaticDraw) 

    attr <- get (attribLocation prog $ idLabel id)
    let numComps = fromIntegral $ numComponents val
    let intHandling = case getGlslType val of
            Int -> KeepIntegral 
            UnsignedInt -> KeepIntegral 
            Byte -> KeepIntegral 
            _ -> ToFloat
    vertexAttribPointer attr $=
        (intHandling, VertexArrayDescriptor numComps (getGlslType val) 0 (makeOff 0))
    vertexAttribArray attr $= Enabled

bindIndices :: Maybe [ConstExpr UInt] -> IO ()
bindIndices (Just inds) = do
    elementArrayBuffer <- genObjectName
    bindBuffer ElementArrayBuffer $= Just elementArrayBuffer
    let indSize = fromIntegral $ 4 * length inds
    withArray (map constEval inds) $ \ptr ->
        bufferData ElementArrayBuffer $= (indSize, ptr, StaticDraw)
bindIndices _ = return ()

getNumElts :: Set.Set InpVar -> Int
getNumElts xs = assert (all (\x -> inpLen x == n) xs) n where
    inpLen (InpVar _ dat) = length dat
    n = inpLen $ head $ Set.toList xs

makeOff :: Int -> Ptr a
makeOff = wordPtrToPtr . fromIntegral