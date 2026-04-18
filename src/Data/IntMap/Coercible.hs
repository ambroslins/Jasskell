module Data.IntMap.Coercible
  ( IntMap,
    empty,
    singleton,
    insert,
    lookup,
    delete,
    update,
    alter,
    alterF,
    fromList,
    toList,
    keys,
  )
where

import Data.Coerce (Coercible, coerce)
import Data.IntMap.Strict qualified as Impl
import Prelude hiding (lookup)

newtype IntMap k v = IntMap (Impl.IntMap v)
  deriving newtype (Eq, Show, Ord, Functor, Foldable)

deriving instance Traversable (IntMap k)

empty :: IntMap k v
empty = IntMap Impl.empty

singleton :: (Coercible Int k) => k -> v -> IntMap k v
singleton k v = IntMap $ Impl.singleton (coerce k) v

insert :: (Coercible Int k) => k -> v -> IntMap k v -> IntMap k v
insert k v (IntMap m) = IntMap $ Impl.insert (coerce k) v m

lookup :: (Coercible Int k) => k -> IntMap k v -> Maybe v
lookup k (IntMap m) = Impl.lookup (coerce k) m

delete :: (Coercible Int k) => k -> IntMap k v -> IntMap k v
delete k (IntMap m) = IntMap $ Impl.delete (coerce k) m

update :: (Coercible Int k) => (v -> Maybe v) -> k -> IntMap k v -> IntMap k v
update f k (IntMap m) = IntMap $ Impl.update f (coerce k) m

alter :: (Coercible Int k) => (Maybe v -> Maybe v) -> k -> IntMap k v -> IntMap k v
alter f k (IntMap m) = IntMap $ Impl.alter f (coerce k) m

alterF :: (Functor f, Coercible Int k) => (Maybe v -> f (Maybe v)) -> k -> IntMap k v -> f (IntMap k v)
alterF f k (IntMap m) = IntMap <$> Impl.alterF f (coerce k) m

fromList :: (Coercible Int k) => [(k, v)] -> IntMap k v
fromList = IntMap . Impl.fromList . coerce

toList :: (Coercible Int k) => IntMap k v -> [(k, v)]
toList (IntMap m) = coerce $ Impl.toList m

keys :: (Coercible Int k) => IntMap k v -> [k]
keys (IntMap m) = coerce $ Impl.keys m
