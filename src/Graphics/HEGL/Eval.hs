{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Graphics.HEGL.Eval (
    constEval,
    hostEval
) where

import Control.Monad.State.Lazy
import Data.Functor.Identity

import Graphics.HEGL.Numerical
import Graphics.HEGL.GLType
import Graphics.HEGL.GLExpr

import qualified Graphics.HEGL.Util.DepMap as DepMap


type IOEvaluator = forall t. GLExpr HostDomain t -> IO t


constEval :: GLExpr ConstDomain t -> t
constEval expr = runIdentity $
    evalStateT (cachedEval eval expr) (EvalState DepMap.empty)

hostEval ::  IOEvaluator -> GLExpr HostDomain t -> IO t
hostEval ioev expr = 
    evalStateT (cachedEval (ioEval ioev) expr) (EvalState DepMap.empty)


data EvalState d = EvalState {
    cache :: DepMap.DepMap (GLExpr d) Identity
}

instance DepMap.GenHashable (GLExpr d) where
    genHash = glExprID

cachedEval :: Monad a => (GLExpr d t -> StateT (EvalState d) a t) ->
    GLExpr d t -> StateT (EvalState d) a t
cachedEval evfn expr = do
    c <- gets cache
    case DepMap.lookup expr c of
        Just (Identity val) -> return val
        Nothing -> do
            val :: t <- evfn expr
            modify (\s -> s { cache = DepMap.insert expr (Identity val) (cache s) })
            return val 


ioEval :: IOEvaluator -> GLExpr HostDomain t -> StateT (EvalState HostDomain) IO t

ioEval ioev e@(GLAtom _ (IOFloat _)) = lift $ ioev e
ioEval _ e = eval e


eval :: Monad a => GLExpr d t -> StateT (EvalState d) a t

eval (GLAtom _ (Const x)) = return x
eval (GLAtom _ (Uniform x)) = undefined
eval (GLAtom _ (Inp _)) = error "Attempted to evaluate an expression in VertexDomain"
eval (GLAtom _ (Frag _)) = error "Attempted to evaluate an expression in FragmentDomain"
eval (GLAtom _ FuncParam) = undefined

eval (GLGenExpr _ (GLVec2 x y)) = do
    x' <- eval x
    y' <- eval y
    return $ x' %| m0 %- y' %| m0
eval (GLGenExpr _ (GLVec3 x y z)) = do
    x' <- eval x
    y' <- eval y
    z' <- eval z
    return $ x' %| m0 %- y' %| m0 %- z' %| m0
    