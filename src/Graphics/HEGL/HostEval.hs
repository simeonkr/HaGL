{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Graphics.HEGL.HostEval (
    hostEval
) where

import Control.Monad.State.Lazy
import Data.Functor.Identity

import qualified Graphics.HEGL.Util.DepMap as DepMap
import Graphics.HEGL.Numerical
import Graphics.HEGL.GLType
import Graphics.HEGL.GLExpr


instance DepMap.GenHashable (GLExpr HostDomain) where
    genHash = glExprId


type IOEvaluator = forall t0. GLType t0 => GLExpr HostDomain t0 -> IO t0


hostEval ::  GLType t => IOEvaluator -> GLExpr HostDomain t -> IO t
hostEval ioev expr = evalStateT (cachedEval expr) (EvalState ioev DepMap.empty)


data EvalState = EvalState {
    ioEvaluator :: IOEvaluator,
    cache :: DepMap.DepMap (GLExpr HostDomain) Identity
}


cachedEval :: GLExpr HostDomain t -> StateT EvalState IO t
cachedEval expr = do
    c <- gets cache
    case DepMap.lookup expr c of
        Just (Identity val) -> return val
        Nothing -> do
            val :: t <- eval expr
            modify (\s -> s { cache = DepMap.insert expr (Identity val) (cache s) })
            return val


eval :: GLExpr HostDomain t -> StateT EvalState IO t

eval (GLAtom _ (Const x)) = return x
eval (GLAtom _ (Uniform x)) = undefined
eval (GLAtom _ (Inp _)) = error "Attempted to evaluate in VertexDomain"
eval (GLAtom _ (Frag _)) = error "Attempted to evaluate in FragmentDomain"
eval (GLAtom _ FuncParam) = undefined

eval (GLGenExpr _ (GLVec2 x y)) = do
    x' <- cachedEval x
    y' <- cachedEval y
    return $ x' %| m0 %- y' %| m0
eval (GLGenExpr _ (GLVec3 x y z)) = do
    x' <- cachedEval x
    y' <- cachedEval y
    z' <- cachedEval z
    return $ x' %| m0 %- y' %| m0 %- z' %| m0
    