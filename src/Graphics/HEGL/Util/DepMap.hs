{-# LANGUAGE RankNTypes #-}

module Graphics.HEGL.Util.DepMap (
    GenHashable(..),
    DepMap,
    Graphics.HEGL.Util.DepMap.empty,
    Graphics.HEGL.Util.DepMap.insert,
    Graphics.HEGL.Util.DepMap.lookup) 
where

import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable(..))
import Unsafe.Coerce (unsafeCoerce)


class GenHashable k where
    genHash :: k t -> Int


data DepMap :: (* -> *) -> (* -> *) -> * where
    DepMap :: GenHashable k => HashMap.HashMap (DMK k) (DMV v) -> DepMap k v

data DMK k where
    DMK :: GenHashable k => k t -> DMK k

instance Eq (DMK k) where
    (DMK key1) == (DMK key2) = genHash key1 == genHash key2 

instance Hashable (DMK k) where
    hashWithSalt salt (DMK key) = hashWithSalt salt $ genHash key

data DMV v where
    DMV :: v t -> DMV v


empty :: GenHashable k => DepMap k v
empty = DepMap (HashMap.empty :: HashMap.HashMap (DMK k) (DMV v))

insert :: GenHashable k => k t -> v t -> DepMap k v -> DepMap k v
insert key val (DepMap em) = DepMap $ HashMap.insert (DMK key) (DMV val) em

lookup :: GenHashable k => k t -> DepMap k v -> Maybe (v t)
lookup key (DepMap em) = HashMap.lookup (DMK key) em >>= 
    \(DMV val) -> return $ unsafeCoerce val

{-data Expr t where
    IntExpr :: Int -> Expr Int
    BoolExpr :: Bool -> Expr Bool

instance GenHashable Expr where
    genHash (IntExpr x) = x
    genHash (BoolExpr False) = 0
    genHash (BoolExpr True) = 1

myMap :: DepMap Expr Identity
myMap = insert (IntExpr 0) 0 $ insert (BoolExpr True) (Identity True) $ empty-}
