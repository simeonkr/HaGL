{-# LANGUAGE RankNTypes #-}

module Graphics.HEGL.Numerical (
    Mat,
    Vec,
    RowVec,
    fromMapping,
    fromList
) where

import GHC.TypeLits
import Data.Array

data Mat :: Nat -> Nat -> * -> * where
    Mat :: Array (Int, Int) t -> Mat p q t

type Vec n t = Mat n 1 t
type RowVec n t = Mat 1 n t

fromMapping :: forall p q t. (KnownNat p, KnownNat q) => ((Int, Int) -> t) -> Mat p q t
fromMapping = undefined

fromList :: forall p q t. (KnownNat p, KnownNat q) => [t] -> Mat p q t
fromList = undefined
