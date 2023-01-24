module Graphics.HEGL.Util.Types (
    Max,
    Min,
    FinList(..),
    flToList
) where

import GHC.TypeNats

-- pilfered from base-4.16.0.0 (Data.Type.Ord)

type family OrdCond (o :: Ordering) (lt :: Nat) (eq :: Nat) (gt :: Nat) where
    OrdCond 'LT lt eq gt = lt
    OrdCond 'EQ lt eq gt = eq
    OrdCond 'GT lt eq gt = gt

type Max (m :: Nat) (n :: Nat) = OrdCond (CmpNat m n) n n m

type Min (m :: Nat) (n :: Nat) = OrdCond (CmpNat m n) m m n

data FinList (n :: Nat) t where
    FLNil :: FinList 0 t
    FLCons :: t -> FinList n t -> FinList (n + 1) t

flToList :: FinList n t -> [t]
flToList FLNil = []
flToList (FLCons x xs) = x : flToList xs
