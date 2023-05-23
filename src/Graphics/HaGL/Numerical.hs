{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Graphics.HaGL.Numerical (
    Mat, Vec, RowVec,
    m0, (%|), (%-),
    horConcat, vertConcat,
    toList, 
    fromMapping,
    fromList,
    eltAt, eltsAt, matCol,
    Graphics.HaGL.Numerical.length,
    distance,
    dot,
    cross,
    normalize,
    transpose,
    matMult,
    outerProduct,
    determinant,
    inverse
) where

import Data.Array
import Data.List (intercalate, length)
import Data.Proxy
import GHC.TypeLits

import qualified Data.Matrix as Mat (fromList, toList, inverse, detLU)

import Graphics.HaGL.Util.Types (FinList, flToList)
import Graphics.HaGL.Util (warn)

-- | A matrix with @p@ rows, @q@ columns, and element type @t@
data Mat (p :: Nat) (q :: Nat) (t :: *) where
    Mat :: Array (Int, Int) t -> Mat p q t
    
-- | A column vector with @n@ elements and element type @t@
type Vec n t = Mat n 1 t

type RowVec n t = Mat 1 n t

m0 :: Mat 1 0 t
m0 = Mat $ array ((0, 0),(-1, -1)) []

infixr 9 %|
infixr 8 %-

(%|) :: t -> RowVec q t -> RowVec (q + 1) t
x %| Mat xs = Mat $ listArray ((0, 0), (0, q + 1)) (x : elems xs)  where
    ((0, 0), (_, q)) = bounds xs

(%-) :: RowVec q t -> Mat p q t -> Mat (p + 1) q t
Mat xs %- Mat ys = Mat $ listArray ((0, 0), (p + 1, q)) (elems xs ++ elems ys) where
    ((0, 0), (p, q)) = bounds ys

horConcat :: Mat p q1 t -> Mat p q2 t -> Mat p (q1 + q2) t
horConcat m1 m2 = transpose (vertConcat (transpose m1) (transpose m2))

vertConcat :: Mat p1 q t -> Mat p2 q t -> Mat (p1 + p2) q t
vertConcat (Mat xs) (Mat ys) = Mat $ listArray ((0, 0), (p1 + p2 + 1, q)) (elems xs ++ elems ys) where
    ((0, 0), (p1, q)) = bounds xs
    ((0, 0), (p2, _)) = bounds ys

instance Eq t => Eq (Mat p q t) where
    (Mat xs) == (Mat ys) = xs == ys

instance Functor (Mat p q) where
    fmap f (Mat xs) = Mat (fmap f xs)

instance Foldable (Mat p q) where
    foldr f z (Mat xs) = foldr f z xs

instance (KnownNat p, KnownNat q) => Applicative (Mat p q) where
    pure x = let p = fromInteger $ natVal (Proxy :: Proxy p)
                 q = fromInteger $ natVal (Proxy :: Proxy q) 
             in Mat $ listArray ((0, 0),(p-1, q-1)) (replicate (p*q) x)
    Mat fs <*> Mat xs = Mat $ listArray (bounds fs) 
        [(fs ! ind) (xs ! ind) | ind <- range (bounds fs)]

instance (KnownNat p, KnownNat q, Num t) => Num (Mat p q t) where
    m1 + m2 = (+) <$> m1 <*> m2
    m1 - m2 = (-) <$> m1 <*> m2
    m1 * m2 = (*) <$> m1 <*> m2
    negate = fmap negate
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger

instance (KnownNat p, KnownNat q, Fractional t) => Fractional (Mat p q t) where
    m1 / m2 = (/) <$> m1 <*> m2
    fromRational = pure . fromRational

-- TODO: Integral support?

instance Show t => Show (Mat p q t) where
    show (Mat xs) = intercalate "\n" [showRow i | i <- [0..p]] ++ "\n" where
        showRow i = intercalate " " [showElt i j | j <- [0..q]]
        showElt i j = show $ xs ! (i, j)
        ((0, 0), (p, q)) = bounds xs


toList :: Mat p q t -> [t]
toList (Mat xs) = elems xs

fromMapping :: forall p q t. (KnownNat p, KnownNat q) => ((Int, Int) -> t) -> Mat p q t
fromMapping f =
    let p = fromInteger $ natVal (Proxy :: Proxy p)
        q = fromInteger $ natVal (Proxy :: Proxy q)  
    in Mat $ array ((0, 0), (p-1, q-1))
        [(ind, f ind) | ind <- range ((0, 0), (p-1, q-1))]

fromList :: forall p q t. (KnownNat p, KnownNat q) => [t] -> Mat p q t
fromList xs =
    let p = fromInteger $ natVal (Proxy :: Proxy p)
        q = fromInteger $ natVal (Proxy :: Proxy q)  
    in Mat $ listArray ((0, 0), (p-1, q-1)) xs

eltAt :: Mat p q t -> (Int, Int) -> t
eltAt (Mat xs) (i, j) = xs ! (i, j)

eltsAt :: Mat p q t -> FinList m (Int, Int) -> Vec m t
eltsAt (Mat xs) fl = Mat $ listArray ((0, 0), (m-1, 0)) vs where
    vs = [xs ! ind | ind <- inds]
    inds = flToList fl
    m = Data.List.length inds

matCol :: Mat p q t -> Int -> Vec p t
matCol (Mat xs) j = Mat $ listArray ((0, 0), (p, 0)) vs where
    vs = [xs ! (i, j) | i <- [0..p]]
    ((0, 0), (p, _)) = bounds xs


length :: Floating t => Vec n t -> t
length (Mat xs) = sqrt $ sum
    [(xs ! (i, 0)) ** 2 | (i, 0) <- range $ bounds xs]

distance :: Floating t => Vec n t -> Vec n t -> t
distance (Mat xs) (Mat ys) = sqrt $ sum
    [((xs ! (i, 0)) - (ys ! (i, 0))) ** 2 | (i, 0) <- range $ bounds xs]

dot :: Num t => Vec n t -> Vec n t -> t
dot (Mat xs) (Mat ys) = 
    sum [(xs ! (i, 0)) * (ys ! (i, 0)) | (i, 0) <- range $ bounds xs]

cross :: Num t => Vec 3 t -> Vec 3 t -> Vec 3 t
cross (Mat xs) (Mat ys) = Mat $ listArray (bounds xs) zs where
    zs = [(xs ! (1, 0)) * (ys ! (2, 0)) - (xs ! (2, 0)) * (ys ! (1, 0)),
          (xs ! (0, 0)) * (ys ! (2, 0)) - (xs ! (2, 0)) * (ys ! (0, 0)),
          (xs ! (0, 0)) * (ys ! (1, 0)) - (xs ! (1, 0)) * (ys ! (0, 0))]

normalize :: Floating t => Vec n t -> Vec n t
normalize v@(Mat xs) = 
    Mat $ listArray (bounds xs) (map (/ Graphics.HaGL.Numerical.length v) (elems xs))


transpose :: Mat p q t -> Mat q p t
transpose (Mat xs) = Mat $ array ((0, 0), (q, p)) xst where
    xst =  [((j, i), xs ! (i, j)) | (i, j) <- range ((0, 0), (p, q))]
    ((0, 0), (p, q)) = bounds xs

matMult :: Num t => Mat p q t -> Mat q r t -> Mat p r t
matMult (Mat xs) (Mat ys) = Mat $ array ((0, 0), (p, r)) zs where
    zs = [((i, j), matElt i j) | (i, j) <- range ((0, 0), (p, r))]
    matElt i j = sum [(xs ! (i, k)) * (ys ! (k, j)) | k <- [0..q]]
    ((0, 0), (p, q)) = bounds xs
    ((0, 0), (_, r)) = bounds ys

outerProduct :: Num t => Vec p t -> Vec q t -> Mat p q t
outerProduct (Mat xs) (Mat ys) = Mat $ array ((0, 0), (p, q)) zs where
    zs = [((i, j), (xs ! (i, 1)) * (ys ! (j, 1))) | (i, j) <- range ((0, 0), (p, q))]
    ((0, 0), (p, 1)) = bounds xs
    ((0, 0), (q, 1)) = bounds ys

determinant :: (Fractional t, Ord t) => Mat p p t -> t
determinant (Mat xs) = Mat.detLU $ Mat.fromList (p + 1) (p + 1) (elems xs) where
    ((0, 0), (p, _)) = bounds xs

inverse :: (Fractional t, Eq t) => Mat p p t -> Mat p p t
inverse (Mat xs) =
    case Mat.inverse $ Mat.fromList (p + 1) (p + 1) (elems xs) of
        Right m -> Mat $ listArray ((0, 0), (p, p)) $ Mat.toList m
        Left msg -> warn msg (Mat xs)
    where ((0, 0), (p, _)) = bounds xs
