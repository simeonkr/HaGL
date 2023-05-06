{-# OPTIONS_GHC -Wno-orphans #-}

module Graphics.HEGL.Print () where

import Prelude hiding (id)
import Control.Monad.State.Lazy (State, execState, gets, modify)
import qualified Data.Set as Set

import Graphics.HEGL.GLType
import Graphics.HEGL.GLExpr
import Graphics.HEGL.ExprID
import Graphics.HEGL.GLAst
import Graphics.HEGL.Eval


-- GLAst printers for debugging purposes

instance (IsGLDomain d, GLType t) => Show (GLExpr d t) where
    show = show . toGLAst

instance Show GLAst where
    show = runPrinter . printGLAst

instance Show GLDomain where
    show ConstDomain = "const"
    show HostDomain = "host"
    show VertexDomain = "vert"
    show FragmentDomain = "frag"

-- set to 0 for no limit
maxDepth = 16

data PrintState = PrintState {
    depth :: Int,
    traversedIds :: Set.Set ExprID,
    buf :: String
}

type Printer = State PrintState ()

runPrinter :: Printer -> String
runPrinter pr = buf $ execState pr 
    PrintState { depth = 0, traversedIds = Set.empty, buf = "" }

printGLAst :: GLAst -> Printer
printGLAst (GLAstAtom id ty (Const _)) =
    printNode id ty "const"
printGLAst (GLAstAtom id ty GenVar) =
    printNode id ty "genVar"
printGLAst (GLAstAtom id ty (Uniform x)) = do
    printNode id ty "uniform"
    ifNotTraversed id $
        indented $ printGLAst $ toGLAst x
printGLAst (GLAstAtom id ty (Inp _)) =
    printNode id ty "inp"
printGLAst (GLAstAtom id ty (Frag _ x)) = do
    printNode id ty "frag"
    ifNotTraversed id $
        indented $ printGLAst $ toGLAst x
printGLAst (GLAstAtom id ty (IOFloat _)) =
    printNode id ty "ioFloat"
printGLAst (GLAstAtom id ty (IODouble _)) =
    printNode id ty "ioDouble"
printGLAst (GLAstAtom id ty (IOInt _)) =
    printNode id ty "ioInt"
printGLAst (GLAstAtom id ty (IOUInt _)) =
    printNode id ty "ioUInt"
printGLAst (GLAstAtom id ty (IOBool _)) =
    printNode id ty "ioBool"
printGLAst (GLAstAtom id ty (IOPrec x0 x)) = do
    printNode id ty "ioPrec"
    ifNotTraversed id $ do
        indented $ printGLAst $ toGLAst x0
        indented $ printGLAst $ toGLAst x
printGLAst (GLAstAtom id ty _) =
    printNode id ty "glLift"
printGLAst (GLAstFunc id ty r params) = do
    printNode id ty "glFunc"
    ifNotTraversed id $ do
        indented $ printGLAst r
        indented $ mapM_ printGLAst params
printGLAst (GLAstFuncApp id ty fn args) = do
    printNode id ty "glFunc app"
    ifNotTraversed id $ do
        indented $ printGLAst fn
        indented $ mapM_ printGLAst args
printGLAst (GLAstExpr id ty op xs) = do
    printNode id ty op
    ifNotTraversed id $
        indented $ mapM_ printGLAst xs

printNode :: ExprID -> GLTypeInfo -> String -> Printer
printNode id ty str = do
    printLine $ str ++ " " ++ idLabel id ++ " : " ++ 
        show (shaderType ty) ++ " " ++ exprType ty

printStr :: String -> Printer
printStr s = do
    depth <- gets depth
    modify (\ps -> ps { buf = buf ps ++ replicate (2 * depth) ' ' ++ s })

printLine :: String -> Printer
printLine = printStr . (++ "\n")

indented :: Printer -> Printer
indented printer = do
    d <- gets depth
    if maxDepth > 0 && d > maxDepth then printLine "  ..." else do
        modify (\ps -> ps { depth = depth ps + 1 })
        printer
        modify (\ps -> ps { depth = depth ps - 1 })

ifNotTraversed :: ExprID -> Printer -> Printer
ifNotTraversed id printAction = do
    ids <- gets traversedIds
    if id `elem` ids then
        printLine "  ..."
    else do
        modify (\ps -> ps { traversedIds = Set.insert id (traversedIds ps) })
        printAction
