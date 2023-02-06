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


type HostVarEvaluator = forall t0. GLType t0 => HostVar t0 -> IO t0


hostEval ::  GLType t => HostVarEvaluator -> GLExpr HostDomain t -> IO t
hostEval hve expr = evalStateT (cachedEval expr) (EvalState hve DepMap.empty)


data EvalState = EvalState {
    hostVarEvaluator :: HostVarEvaluator,
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

eval (GLExpr id (Const x)) = return x
eval (GLExpr id (Uniform x)) = undefined
eval (GLExpr id (HostVar x)) = undefined
eval (GLExpr id (Inp x)) = error "Attempted to evaluate in VertexDomain"
eval (GLExpr id (Frag x)) = error "Attempted to evaluate in FragmentDomain"
eval (GLExpr id FuncParam) = undefined

eval (GLExpr id (GLVec2 x y)) = do
    x' <- cachedEval x
    y' <- cachedEval y
    return $ x' %| m0 %- y' %| m0
eval (GLExpr id (GLVec3 x y z)) = do
    x' <- cachedEval x
    y' <- cachedEval y
    z' <- cachedEval z
    return $ x' %| m0 %- y' %| m0 %- z' %| m0
    