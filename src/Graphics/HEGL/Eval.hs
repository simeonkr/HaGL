{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Graphics.HEGL.Eval (
    constEval,
    hostEval
) where

import Prelude hiding (length)

import Control.Applicative (liftA2)
import Control.Monad.State.Lazy
import Data.Functor.Identity
import Data.Bits

import Graphics.HEGL.Numerical
import Graphics.HEGL.GLType
import Graphics.HEGL.GLExpr
import Graphics.HEGL.ExprID
import Graphics.HEGL.Util.Types (FinList(..))

import qualified Graphics.HEGL.Util.DepMap as DepMap


type IOEvaluator = forall t. GLExpr HostDomain t -> IO t


constEval :: GLExpr ConstDomain t -> t
constEval expr = runIdentity $
    evalStateT (cachedEval eval expr) (EvalState DepMap.empty)

hostEval ::  IOEvaluator -> GLExpr HostDomain t -> IO t
hostEval ioev expr = 
    evalStateT (cachedEval (hEval ioev) expr) (EvalState DepMap.empty)


data EvalState d = EvalState {
    cache :: DepMap.DepMap (GLExpr d) Identity
}

instance DepMap.GenHashable (GLExpr d) where
    genHash = getID

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


hEval :: IOEvaluator -> GLExpr HostDomain t -> StateT (EvalState HostDomain) IO t

hEval ioev e@(GLAtom _ (IOFloat _)) = lift $ ioev e
hEval ioev e@(GLAtom _ (IODouble _)) = lift $ ioev e
hEval ioev e@(GLAtom _ (IOInt _)) = lift $ ioev e
hEval ioev e@(GLAtom _ (IOUInt _)) = lift $ ioev e
hEval ioev e@(GLAtom _ (IOBool _)) = lift $ ioev e
hEval ioev e@(GLAtom _ (IOPrec _ _)) = lift $ ioev e
hEval ioev (GLAtom _ (Uniform x)) = hEval ioev x 
hEval _ e = eval e


eval :: Monad a => GLExpr d t -> StateT (EvalState d) a t

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

eval (GLFunc _ (GLFunc1 f _ x0)) = eval $ f x0
eval (GLFunc _ (GLFunc2 f _ _ x0 y0)) = eval $ f x0 y0
eval (GLFunc _ (GLFunc3 f _ _ _ x0 y0 z0)) = eval $ f x0 y0 z0

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
eval (GLGenExpr _ (HorConc xs ys)) = withEv2 xs ys $ \xs ys ->
    return $ horConcat xs ys
eval (GLGenExpr _ (GLArray xs)) = 
    mapM (cachedEval eval) xs

eval (GLGenExpr _ (OpCoord coords v)) = withEv1 v $ \v ->
    return $ v `eltAt` coordToIndex coords
eval (GLGenExpr _ (OpCoordMulti coordList v)) = withEv1 v $ \v ->
    return $ v `eltsAt` (toFinList coordList) where
        toFinList :: GLCoordList l m -> FinList l (Int, Int)
        toFinList CoordNil = FLNil
        toFinList (CoordCons c cs) = FLCons (coordToIndex c) (toFinList cs)
eval (GLGenExpr _ (OpCol col m)) = withEv1 m $ \m ->
    return $ m `matCol` colToIndex col
eval (GLGenExpr _ (OpArrayElt arr i)) = withEv2 arr i $ \arr i ->
    return $ arr !! fromIntegral i

eval (GLGenExpr _ (Cast x)) = withEv1 x $ \x ->
    return $ cast x

eval (GLGenExpr _ (OpAdd x y)) = withEv2 x y $ \x y -> 
    return $ glZipWith (+) x y
eval (GLGenExpr _ (OpSubt x y)) = withEv2 x y $ \x y -> 
    return $ glZipWith (-) x y
eval (GLGenExpr _ (OpMult x y)) = withEv2 x y $ \x y -> 
    return $ glZipWith (*) x y
eval (GLGenExpr _ (OpDiv x y)) = withEv2 x y $ \x y ->
    return $ glZipWith genDiv x y
-- TODO: warn if one of the operands is negative or second operand is zero
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
eval (GLGenExpr _ (OpCond x y z)) = withEv3 x y z $ \x y z -> 
    return $ if x then y else z
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
    return $ glMap ((*) x) y
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

eval (GLGenExpr _ (Pow x y)) = withEv2 x y $ \x y -> 
    return $ glZipWith (**) x y
eval (GLGenExpr _ (Exp x)) = withEv1 x $ \x -> 
    return $ glMap exp x
eval (GLGenExpr _ (Log x)) = withEv1 x $ \x -> 
    return $ glMap log x
eval (GLGenExpr _ (Exp2 x)) = withEv1 x $ \x -> 
    return $ glMap ((**) 2) x
eval (GLGenExpr _ (Log2 x)) = withEv1 x $ \x -> 
    return $ glMap (logBase 2) x
eval (GLGenExpr _ (Sqrt x)) = withEv1 x $ \x -> 
    return $ glMap sqrt x

eval (GLGenExpr _ (Abs x)) = withEv1 x $ \x -> 
    return $ glMap abs x
eval (GLGenExpr _ (Sign x)) = withEv1 x $ \x -> 
    return $ glMap signum x
eval (GLGenExpr _ (Floor x)) = withEv1 x $ \x -> 
    return $ glMap (fromIntegral . floor) x
eval (GLGenExpr _ (Trunc x)) = withEv1 x $ \x -> 
    return $ glMap (fromIntegral . truncate) x
eval (GLGenExpr _ (Round x)) = withEv1 x $ \x -> 
    return $ glMap (fromIntegral . round) x
eval (GLGenExpr _ (RoundEven x)) = withEv1 x $ \x -> 
    return $ glMap (fromIntegral . round) x
eval (GLGenExpr _ (Ceil x)) = withEv1 x $ \x -> 
    return $ glMap (fromIntegral . ceiling) x
eval (GLGenExpr _ (Fract x)) = withEv1 x $ \x -> 
    return $ glMap (snd . properFraction) x
eval (GLGenExpr _ (Mod x y)) = withEv2 x y $ \x y -> 
    return $ glZipWith mod x y where
        mod x y = x - y * (fromIntegral . floor) (x / y)
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
        clamp x minVal maxVal = min (max x minVal) maxVal
        smoothstep edge0 edge1 x = 
            let t = min (max ((x - edge0) / (edge1 - edge0)) 0) 1 
            in t * t * (3 - 2 * t)

eval (GLGenExpr _ (Length x)) = withEv1 x $ \x -> 
    return $ length $ x
eval (GLGenExpr _ (Distance x y)) = withEv2 x y $ \x y -> 
    return $ x `distance` y
eval (GLGenExpr _ (Dot x y)) = withEv2 x y $ \x y -> 
    return $ x `dot` y
eval (GLGenExpr _ (Cross x y)) = withEv2 x y $ \x y -> 
    return $ x `cross` y
eval (GLGenExpr _ (Normalize x)) = withEv1 x $ \x -> 
    return $ normalize $ x
eval (GLGenExpr _ (Faceforward x y z)) = withEv3 x y z $ \x y z -> 
    return $ faceforward x y z where
        faceforward n i nr = if dot nr i < 0 then n else -n
eval (GLGenExpr _ (Reflect x y)) = withEv2 x y $ \x y -> 
    return $ reflect x y where
    c .* v = glMap (c *) v
    reflect i n = i - 2 * (dot n i) .* n
eval (GLGenExpr _ (Refract x y z)) = withEv3 x y z $ \x y z -> 
    return $ refract x y z where
        c .* v = glMap (c *) v
        refract i n eta =
            let k = 1 - eta * eta * (1 - (dot n i) * (dot n i))
            in if k < 0 then 0 else eta .* i - (eta * (dot n i) + (sqrt k)) .* n

eval (GLGenExpr _ (MatrixCompMult x y)) = withEv2 x y $ \x y -> 
    return $ glZipWith (*) x y
eval (GLGenExpr _ (OuterProduct x y)) = withEv2 x y $ \x y -> 
    return $ x `outerProduct` y
eval (GLGenExpr _ (Transpose x)) = withEv1 x $ \x -> 
    return $ transpose $ x
eval (GLGenExpr _ (Determinant x)) = withEv1 x $ \x -> 
    return $ determinant $ x
eval (GLGenExpr _ (Inverse x)) = withEv1 x $ \x -> 
    return $ inverse $ x

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
eval (GLGenExpr _ (Compl x)) = withEv1 x $ \x -> 
    return $ fmap complement x


-- Helper functions

withEv1 x act = do { x <- cachedEval eval x; act x }
withEv2 x y act = do { x <- cachedEval eval x; y <- cachedEval eval y; act x y }
withEv3 x y z act = do { x <- cachedEval eval x; y <- cachedEval eval y; z <- cachedEval eval z; act x y z }
withEv4 x y z w act = do { x <- cachedEval eval x; y <- cachedEval eval y; z <- cachedEval eval z; w <- cachedEval eval w; act x y z w }

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
