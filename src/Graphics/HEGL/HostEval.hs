{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -dth-dec-file #-}

module Graphics.HEGL.HostEval (
    hostEval
) where

import Control.Monad.State.Lazy
import Data.Functor.Identity

import qualified Graphics.HEGL.Util.DepMap as DepMap
import Graphics.HEGL.Numerical
import Graphics.HEGL.GLType
import Graphics.HEGL.GLExpr
import Graphics.HEGL.TH (genGlExprId)


$genGlExprId

instance DepMap.GenHashable (GLExpr HostDomain) where
    genHash = getExprId


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

eval (Const id x) = return x
eval (Uniform id x) = undefined
eval (HostVar id x) = undefined
eval (Inp id _) = error "Attempted to evaluate in VertexDomain"
eval (Frag id _) = error "Attempted to evaluate in FragmentDomain"
eval (FuncParam id) = undefined

eval (GLVec2 id x y) = do
    x' <- cachedEval x
    y' <- cachedEval y
    return $ x' %| m0 %- y' %| m0
eval (GLVec3 id x y z) = do
    x' <- cachedEval x
    y' <- cachedEval y
    z' <- cachedEval z
    return $ x' %| m0 %- y' %| m0 %- z' %| m0
    