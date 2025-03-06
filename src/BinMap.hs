{-# LANGUAGE DeriveFunctor #-}

module BinMap where

import BinTree
    ( BinTree, empty, singleton, insert, delete, mapMonotonic )
import Data.Void (Void)
import Data.Bifunctor (first)

data Entry k v = Entry { eKey :: k, eValue :: v } -- (k, v)
    deriving (Functor)

instance (Read k, Read v) => Read (Entry k v) where
    readsPrec n s =
        [ (Entry k v, u)
        | (k, ':':t) <- readsPrec n s
        , (v, u) <- readsPrec n t
        ]

instance (Show k, Show v) => Show (Entry k v) where
    show (Entry k v) = show k ++ ":" ++ show v

instance Eq k => Eq (Entry k v) where
    Entry k _ == Entry k' _ = k == k'

instance Ord k => Ord (Entry k v) where
    compare (Entry k _) (Entry k' _) = compare k k'

newtype BinMap k v = BinMap { binMap :: BinTree (Entry k v) }
    deriving (Show)

instance (Read k, Read v) => Read (BinMap k v) where
    readsPrec n s = map (first BinMap) (readsPrec n s)

emptyMap :: BinMap k v
emptyMap = BinMap empty

singletonMap :: k -> v -> BinMap k v
singletonMap k v = BinMap (singleton (Entry k v))

insertMap :: Ord k => k -> v -> BinMap k v -> BinMap k v
insertMap k v (BinMap t) = BinMap (insert (Entry k v) t)

deleteMap :: Ord k => k -> BinMap k v -> BinMap k v
deleteMap k (BinMap t) = BinMap (delete (Entry k undefined) t)

instance Ord k => Semigroup (BinMap k v) where
    BinMap t <> BinMap t' = BinMap (t <> t')

instance Ord k => Monoid (BinMap k v) where
    mempty = emptyMap

instance Functor (BinMap k) where
    fmap f (BinMap t) = BinMap (mapMonotonic (fmap f) t)

unit :: ()
unit = ()

pair :: (Int,String)
pair = (5, "a")

void :: Void
void = error "no such value"

q1 :: [BinTree ()]
q1 = [singleton (), empty]

q2 :: [BinTree Void]
q2 = [empty]