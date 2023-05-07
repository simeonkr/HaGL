{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Graphics.HEGL.Eval (
    constEval,
    hostEval,
    EvalException(..)
) where

import Prelude hiding (id)
import Control.Exception (Exception, throw)
import Control.Applicative (liftA2)
import Control.Monad.State.Lazy
import Data.Functor.Identity
import Data.Bits

import Graphics.HEGL.Numerical
import Graphics.HEGL.GLType
import Graphics.HEGL.GLExpr
import Graphics.HEGL.Util.Types (FinList(..))
import qualified Graphics.HEGL.Util.DepMap as DepMap


type IOEvaluator = forall t. GLExpr HostDomain t -> IO t


data EvalException =
    GenericUniformEval

instance Exception EvalException

instance Show EvalException where
    show GenericUniformEval = "Attempted to evaluate a user-defined uniform variable"


constEval :: GLExpr ConstDomain t -> t
constEval expr = runIdentity $
    evalStateT (cachedEval expr) (EvalState eval DepMap.empty)

hostEval ::  IOEvaluator -> GLExpr HostDomain t -> IO t
hostEval ioev expr = 
    evalStateT (cachedEval expr) (EvalState (hEval ioev) DepMap.empty)


data EvalState d a = EvalState {
    evalfn :: Monad a => forall t. GLExpr d t -> StateT (EvalState d a) a t,
    cache :: DepMap.DepMap (GLExpr d) Identity
}

cachedEval :: Monad a => GLExpr d t -> StateT (EvalState d a) a t
cachedEval expr = do
    evfn <- gets evalfn
    c <- gets cache
    case DepMap.lookup expr c of
        Just (Identity val) -> return val
        Nothing -> do
            val :: t <- evfn expr
            modify (\s -> s { cache = DepMap.insert expr (Identity val) (cache s) })
            return val 


hEval :: IOEvaluator -> GLExpr HostDomain t -> StateT (EvalState HostDomain IO) IO t

hEval ioev e@(GLAtom _ (IOFloat _)) = lift $ ioev e
hEval ioev e@(GLAtom _ (IODouble _)) = lift $ ioev e
hEval ioev e@(GLAtom _ (IOInt _)) = lift $ ioev e
hEval ioev e@(GLAtom _ (IOUInt _)) = lift $ ioev e
hEval ioev e@(GLAtom _ (IOBool _)) = lift $ ioev e
hEval ioev e@(GLAtom _ (IOPrec _ _)) = lift $ ioev e
hEval ioev (GLAtom _ (Uniform x)) = hEval ioev x
hEval ioev (GLAtom _ (GenericUniform _)) = throw GenericUniformEval
hEval _ e = eval e


eval :: Monad a => GLExpr d t -> StateT (EvalState d a) a t

eval (GLAtom _ (Const x)) = return x
eval (GLAtom _ GenVar) = error "Attempted to evaluate an unknown variable"
eval (GLAtom _ (Uniform _)) = error "Attempted to purely evaluate an expression in HostDomain"
eval (GLAtom _ (Inp _)) = error "Attempted to evaluate an expression in VertexDomain"
eval (GLAtom _ (Frag _ _)) = error "Attempted to evaluate an expression in FragmentDomain"
eval (GLAtom _ (GLLift0 x0)) = return x0
eval (GLAtom _ (GLLift1 f x0)) = withEv1 x0 $ \x0 ->
    return $ f x0
eval (GLAtom _ (GLLift2 f x0 y0)) = withEv2 x0 y0 $ \x0 y0 ->
    return $ f x0 y0
eval (GLAtom _ (GLLift3 f x0 y0 z0)) = withEv3 x0 y0 z0 $ \x0 y0 z0 ->
    return $ f x0 y0 z0
eval (GLAtom _ (GLLift4 f x0 y0 z0 w0)) = withEv4 x0 y0 z0 w0 $ \x0 y0 z0 w0 ->
    return $ f x0 y0 z0 w0
eval (GLAtom _ (GLLift5 f x0 y0 z0 w0 u0)) = withEv5 x0 y0 z0 w0 u0 $ \x0 y0 z0 w0 u0 ->
    return $ f x0 y0 z0 w0 u0
eval (GLAtom _ (GLLift6 f x0 y0 z0 w0 u0 v0)) = withEv6 x0 y0 z0 w0 u0 v0 $ \x0 y0 z0 w0 u0 v0 ->
    return $ f x0 y0 z0 w0 u0 v0

eval (GLFunc _ (GLFunc1 f _ x0)) = eval $ f x0
eval (GLFunc _ (GLFunc2 f _ _ x0 y0)) = eval $ f x0 y0
eval (GLFunc _ (GLFunc3 f _ _ _ x0 y0 z0)) = eval $ f x0 y0 z0
eval (GLFunc _ (GLFunc4 f _ _ _ _ x0 y0 z0 w0)) = eval $ f x0 y0 z0 w0
eval (GLFunc _ (GLFunc5 f _ _ _ _ _ x0 y0 z0 w0 u0)) = eval $ f x0 y0 z0 w0 u0
eval (GLFunc _ (GLFunc6 f _ _ _ _ _ _ x0 y0 z0 w0 u0 v0)) = eval $ f x0 y0 z0 w0 u0 v0

eval (GLGenExpr _ (GLVec2 x y)) = withEv2 x y $ \x y -> 
    return $ x %| m0 %- y %| m0
eval (GLGenExpr _ (GLVec3 x y z)) = withEv3 x y z $ \x y z -> 
    return $ x %| m0 %- y %| m0 %- z %| m0
eval (GLGenExpr _ (GLVec4 x y z w)) = withEv4 x y z w $ \x y z w -> 
    return $ x %| m0 %- y %| m0 %- z %| m0 %- w %| m0
eval (GLGenExpr _ (GLMat2x2 x y)) = withEv2 x y $ \x y -> 
    return $ tr $ tr x %- tr y
eval (GLGenExpr _ (GLMat2x3 x y z)) = withEv3 x y z $ \x y z -> 
    return $ tr $ tr x %- tr y %- tr z
eval (GLGenExpr _ (GLMat2x4 x y z w)) = withEv4 x y z w $ \x y z w -> 
    return $ tr $ tr x %- tr y %- tr z %- tr w
eval (GLGenExpr _ (GLMat3x2 x y)) = withEv2 x y $ \x y -> 
    return $ tr $ tr x %- tr y
eval (GLGenExpr _ (GLMat3x3 x y z)) = withEv3 x y z $ \x y z -> 
    return $ tr $ tr x %- tr y %- tr z
eval (GLGenExpr _ (GLMat3x4 x y z w)) = withEv4 x y z w $ \x y z w -> 
    return $ tr $ tr x %- tr y %- tr z %- tr w
eval (GLGenExpr _ (GLMat4x2 x y)) = withEv2 x y $ \x y -> 
    return $ tr $ tr x %- tr y
eval (GLGenExpr _ (GLMat4x3 x y z)) = withEv3 x y z $ \x y z -> 
    return $ tr $ tr x %- tr y %- tr z
eval (GLGenExpr _ (GLMat4x4 x y z w)) = withEv4 x y z w $ \x y z w -> 
    return $ tr $ tr x %- tr y %- tr z %- tr w
eval (GLGenExpr _ (Pre x xs)) = withEv2 x xs $ \x xs ->
    return $ x %| m0 %- xs
eval (GLGenExpr _ (App xs x)) = withEv2 xs x $ \xs x ->
    return $ vertConcat xs (x %| m0)
eval (GLGenExpr _ (Conc xs ys)) = withEv2 xs ys $ \xs ys ->
    return $ vertConcat xs ys
eval (GLGenExpr _ (GLArray xs)) = 
    mapM cachedEval xs

eval (GLGenExpr _ (OpCoord coords v)) = withEv1 v $ \v ->
    return $ v `eltAt` coordToIndex coords
eval (GLGenExpr _ (OpCoordMulti coordList v)) = withEv1 v $ \v ->
    return $ v `eltsAt` toFinList coordList where
        toFinList :: GLCoordList l m -> FinList l (Int, Int)
        toFinList CoordNil = FLNil
        toFinList (CoordCons c cs) = FLCons (coordToIndex c) (toFinList cs)
eval (GLGenExpr _ (OpCol col m)) = withEv1 m $ \m ->
    return $ m `matCol` colToIndex col
eval (GLGenExpr _ (OpArrayElt arr i)) = withEv2 arr i $ \arr i ->
    return $ arr !! fromIntegral (i `mod` Prelude.length arr)

eval (GLGenExpr _ (Cast x)) = withEv1 x $ \x ->
    return $ glCast x
eval (GLGenExpr _ (MatCast x)) = withEv1 x $ \x ->
    return $ fromList . map glCast . toList $ x

eval (GLGenExpr _ (OpAdd x y)) = withEv2 x y $ \x y -> 
    return $ glZipWith (+) x y
eval (GLGenExpr _ (OpSubt x y)) = withEv2 x y $ \x y -> 
    return $ glZipWith (-) x y
eval (GLGenExpr _ (OpMult x y)) = withEv2 x y $ \x y -> 
    return $ glZipWith (*) x y
eval (GLGenExpr _ (OpDiv x y)) = withEv2 x y $ \x y ->
    return $ glZipWith genDiv x y
eval (GLGenExpr _ (OpMod x y)) = withEv2 x y $ \x y -> 
    return $ glZipWith mod x y
eval (GLGenExpr _ (OpNeg x)) = withEv1 x $ \x ->
    return $ glMap negate x
eval (GLGenExpr _ (OpLessThan x y)) = withEv2 x y $ \x y ->
    return $ x < y
eval (GLGenExpr _ (OpLessThanEqual x y)) = withEv2 x y $ \x y ->
    return $ x <= y
eval (GLGenExpr _ (OpGreaterThan x y)) = withEv2 x y $ \x y ->
    return $ x > y
eval (GLGenExpr _ (OpGreaterThanEqual x y)) = withEv2 x y $ \x y ->
    return $ x >= y
eval (GLGenExpr _ (OpEqual x y)) = withEv2 x y $ \x y ->
    return $ x == y
eval (GLGenExpr _ (OpNotEqual x y)) = withEv2 x y $ \x y ->
    return $ x /= y
eval (GLGenExpr _ (OpAnd x y)) = withEv2 x y $ \x y -> 
    return $ x .&. y
eval (GLGenExpr _ (OpOr x y)) = withEv2 x y $ \x y ->
    return $ x .|. y
eval (GLGenExpr _ (OpXor x y)) = withEv2 x y $ \x y ->
    return $ x `xor` y
eval (GLGenExpr _ (OpNot x)) = withEv1 x $ \x -> 
    return $ complement x
eval (GLGenExpr _ (OpCond x y z)) =
    -- this is the only case where we have to be lazy
    eval x >>= \x -> if x then eval y else eval z
eval (GLGenExpr _ (OpCompl x)) = withEv1 x $ \x ->
    return $ glMap complement x
eval (GLGenExpr _ (OpLshift x y)) = withEv2 x y $ \x y -> 
    return $ glZipWith (\x y -> x `shiftL` (fromInteger . toInteger) y) x y 
eval (GLGenExpr _ (OpRshift x y)) = withEv2 x y $ \x y -> 
    return $ glZipWith (\x y -> x `shiftR` (fromInteger . toInteger) y) x y 
eval (GLGenExpr _ (OpBitAnd x y)) = withEv2 x y $ \x y -> 
    return $ glZipWith (.&.) x y 
eval (GLGenExpr _ (OpBitOr x y)) = withEv2 x y $ \x y -> 
    return $ glZipWith (.|.) x y 
eval (GLGenExpr _ (OpBitXor x y)) = withEv2 x y $ \x y -> 
    return $ glZipWith xor x y
eval (GLGenExpr _ (OpScalarMult x y)) = withEv2 x y $ \x y -> 
    return $ glMap (x *) y
eval (GLGenExpr _ (OpMatrixMult x y)) = withEv2 x y $ \x y -> 
    return $ matMult x y

eval (GLGenExpr _ (Radians x)) = withEv1 x $ \x ->
    return $ glMap radians x where
        radians deg = (pi / 180) * deg
eval (GLGenExpr _ (Degrees x)) = withEv1 x $ \x ->
    return $ glMap degrees x where
        degrees rad = (180 / pi) * rad
eval (GLGenExpr _ (Sin x)) = withEv1 x $ \x -> 
    return $ glMap sin x
eval (GLGenExpr _ (Cos x)) = withEv1 x $ \x -> 
    return $ glMap cos x
eval (GLGenExpr _ (Tan x)) = withEv1 x $ \x -> 
    return $ glMap tan x
eval (GLGenExpr _ (Asin x)) = withEv1 x $ \x -> 
    return $ glMap asin x
eval (GLGenExpr _ (Acos x)) = withEv1 x $ \x -> 
    return $ glMap acos x
eval (GLGenExpr _ (Atan x)) = withEv1 x $ \x -> 
    return $ glMap atan x
eval (GLGenExpr _ (Sinh x)) = withEv1 x $ \x -> 
    return $ glMap sinh x
eval (GLGenExpr _ (Cosh x)) = withEv1 x $ \x -> 
    return $ glMap cosh x
eval (GLGenExpr _ (Tanh x)) = withEv1 x $ \x -> 
    return $ glMap tanh x
eval (GLGenExpr _ (Asinh x)) = withEv1 x $ \x -> 
    return $ glMap asinh x
eval (GLGenExpr _ (Acosh x)) = withEv1 x $ \x -> 
    return $ glMap acosh x
eval (GLGenExpr _ (Atanh x)) = withEv1 x $ \x -> 
    return $ glMap atanh x

eval (GLGenExpr _ (Pow x y)) = withEv2 x y $ \x y -> 
    return $ glZipWith (**) x y
eval (GLGenExpr _ (Exp x)) = withEv1 x $ \x -> 
    return $ glMap exp x
eval (GLGenExpr _ (Log x)) = withEv1 x $ \x -> 
    return $ glMap log x
eval (GLGenExpr _ (Exp2 x)) = withEv1 x $ \x -> 
    return $ glMap (2 **) x
eval (GLGenExpr _ (Log2 x)) = withEv1 x $ \x -> 
    return $ glMap (logBase 2) x
eval (GLGenExpr _ (Sqrt x)) = withEv1 x $ \x -> 
    return $ glMap sqrt x
eval (GLGenExpr _ (Inversesqrt x)) = withEv1 x $ \x -> 
    return $ glMap (recip . sqrt) x

eval (GLGenExpr _ (Abs x)) = withEv1 x $ \x -> 
    return $ glMap abs x
eval (GLGenExpr _ (Sign x)) = withEv1 x $ \x -> 
    return $ glMap signum x
eval (GLGenExpr _ (Floor x)) = withEv1 x $ \x -> 
    return $ glMap (fromIntegral . (floor :: _ -> Int)) x
eval (GLGenExpr _ (Trunc x)) = withEv1 x $ \x -> 
    return $ glMap (fromIntegral . (truncate :: _ -> Int)) x
eval (GLGenExpr _ (Round x)) = withEv1 x $ \x -> 
    return $ glMap (fromIntegral . (round :: _ -> Int)) x
eval (GLGenExpr _ (RoundEven x)) = withEv1 x $ \x -> 
    return $ glMap (fromIntegral . (round :: _ -> Int)) x
eval (GLGenExpr _ (Ceil x)) = withEv1 x $ \x -> 
    return $ glMap (fromIntegral . (ceiling :: _ -> Int)) x
eval (GLGenExpr _ (Fract x)) = withEv1 x $ \x -> 
    return $ glMap (snd . (properFraction :: _ -> (Int, _))) x
eval (GLGenExpr _ (Mod x y)) = withEv2 x y $ \x y -> 
    return $ glZipWith mod x y where
        mod x y = x - y * (fromIntegral . (floor :: _ -> Int)) (x / y)
eval (GLGenExpr _ (Min x y)) = withEv2 x y $ \x y -> 
    return $ glZipWith min x y
eval (GLGenExpr _ (Max x y)) = withEv2 x y $ \x y -> 
    return $ glZipWith max x y
eval (GLGenExpr _ (Clamp x y z)) = withEv3 x y z $ \x y z -> 
    return $ glZipWith3 clamp x y z where
        clamp x minVal maxVal = min (max x minVal) maxVal
eval (GLGenExpr _ (Mix x y z)) = withEv3 x y z $ \x y z -> 
    return $ glZipWith3 mix x y z where
        mix x y a = x * (1 - a) + y * a
eval (GLGenExpr _ (Step x y)) = withEv2 x y $ \x y -> 
    return $ glZipWith step x y where
        step edge x = if x < edge then 0 else 1
eval (GLGenExpr _ (Smoothstep x y z)) =  withEv3 x y z $ \x y z -> 
    return $ glZipWith3 smoothstep x y z where
        smoothstep edge0 edge1 x = 
            let t = min (max ((x - edge0) / (edge1 - edge0)) 0) 1 
            in t * t * (3 - 2 * t)

eval (GLGenExpr _ (Length x)) = withEv1 x $ \x -> 
    return $ Graphics.HEGL.Numerical.length x
eval (GLGenExpr _ (Distance x y)) = withEv2 x y $ \x y -> 
    return $ x `distance` y
eval (GLGenExpr _ (Dot x y)) = withEv2 x y $ \x y -> 
    return $ x `dot` y
eval (GLGenExpr _ (Cross x y)) = withEv2 x y $ \x y -> 
    return $ x `cross` y
eval (GLGenExpr _ (Normalize x)) = withEv1 x $ \x -> 
    return $ normalize x
eval (GLGenExpr _ (Faceforward x y z)) = withEv3 x y z $ \x y z -> 
    return $ faceforward x y z where
        faceforward n i nr = if dot nr i < 0 then n else -n
eval (GLGenExpr _ (Reflect x y)) = withEv2 x y $ \x y -> 
    return $ reflect x y where
    c .# v = glMap (c *) v
    reflect i n = i - 2 * dot n i .# n
eval (GLGenExpr _ (Refract x y z)) = withEv3 x y z $ \x y z -> 
    return $ refract x y z where
        c .# v = glMap (c *) v
        refract i n eta =
            let k = 1 - eta * eta * (1 - dot n i * dot n i)
            in if k < 0 then 0 else eta .# i - (eta * dot n i + sqrt k) .# n

eval (GLGenExpr _ (MatrixCompMult x y)) = withEv2 x y $ \x y -> 
    return $ glZipWith (*) x y
eval (GLGenExpr _ (OuterProduct x y)) = withEv2 x y $ \x y -> 
    return $ x `outerProduct` y
eval (GLGenExpr _ (Transpose x)) = withEv1 x $ \x -> 
    return $ transpose x
eval (GLGenExpr _ (Determinant x)) = withEv1 x $ \x -> 
    return $ determinant x
eval (GLGenExpr _ (Inverse x)) = withEv1 x $ \x -> 
    return $ inverse x

eval (GLGenExpr _ (LessThan x y)) = withEv2 x y $ \x y -> 
    return $ liftA2 (<) x y
eval (GLGenExpr _ (LessThanEqual x y)) = withEv2 x y $ \x y -> 
    return $ liftA2 (<=) x y
eval (GLGenExpr _ (GreaterThan x y)) = withEv2 x y $ \x y -> 
    return $ liftA2 (>) x y
eval (GLGenExpr _ (GreaterThanEqual x y)) = withEv2 x y $ \x y -> 
    return $ liftA2 (>=) x y
eval (GLGenExpr _ (Equal x y)) = withEv2 x y $ \x y -> 
    return $ liftA2 (==) x y
eval (GLGenExpr _ (NotEqual x y)) = withEv2 x y $ \x y -> 
    return $ liftA2 (/=) x y
eval (GLGenExpr _ (Any x)) = withEv1 x $ \x -> 
    return $ foldr (.|.) False x
eval (GLGenExpr _ (All x)) = withEv1 x $ \x -> 
    return $ foldr (.&.) True x
eval (GLGenExpr _ (Not x)) = withEv1 x $ \x -> 
    return $ fmap complement x


-- Helper functions

withEv1 x act = do { x <- cachedEval x; act x }
withEv2 x y act = do { x <- cachedEval x; y <- cachedEval y; act x y }
withEv3 x y z act = do { x <- cachedEval x; y <- cachedEval y; z <- cachedEval z; act x y z }
withEv4 x y z w act = do { x <- cachedEval x; y <- cachedEval y; z <- cachedEval z; w <- cachedEval w; act x y z w }
withEv5 x y z w u act = do { x <- cachedEval x; y <- cachedEval y; z <- cachedEval z; w <- cachedEval w; u <- cachedEval u; act x y z w u }
withEv6 x y z w u v act = do { x <- cachedEval x; y <- cachedEval y; z <- cachedEval z; w <- cachedEval w; u <- cachedEval u; v <- cachedEval v; act x y z w u v }

tr = transpose

coordToIndex :: GLCoord m -> (Int, Int)
coordToIndex CoordX = (0, 0)
coordToIndex CoordY = (1, 0)
coordToIndex CoordZ = (2, 0)
coordToIndex CoordW = (3, 0)

colToIndex :: GLCol m -> Int
colToIndex Col0 = 0
colToIndex Col1 = 1
colToIndex Col2 = 2
colToIndex Col3 = 3
