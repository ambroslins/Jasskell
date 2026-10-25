{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FieldSelectors #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- HLINT ignore "Redundant lambda" -}

module Data.Vec
  ( Vec,
    generate,
    replicate,
    index,
    set,
    modify,
    findIndex,
    coerce,
    map,
    rotate,
    foldl',
    ifoldl',
    foldr,
    ifoldr,
    all,
    toList,
    fromList,
    generateA,
    mapM,
    sequence,
    iforM_,
    Boxed (..),
    Elem (..),
  )
where

import Control.Monad.ST (ST, runST)
import Data.Coerce (Coercible)
import Data.Fin (Fin, nat)
import Data.Fin qualified as Fin
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Type)
import Data.Primitive (Prim, sizeOf)
import Data.Primitive.ByteArray
  ( ByteArray,
    MutableByteArray,
    copyByteArray,
    indexByteArray,
    newByteArray,
    sizeofByteArray,
    unsafeFreezeByteArray,
    writeByteArray,
  )
import Data.Primitive.SmallArray
  ( SmallArray,
    SmallMutableArray,
    copySmallArray,
    indexSmallArray,
    newSmallArray,
    sizeofSmallArray,
    unsafeFreezeSmallArray,
    writeSmallArray,
  )
import Data.Text (Text)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.List qualified as List
import GHC.TypeLits (Nat)
import GHC.TypeNats (KnownNat)
import Prelude hiding
  ( all,
    foldl',
    foldr,
    map,
    mapM,
    replicate,
    sequence,
  )

class Elem a where
  type Store a :: Type

  type MutableStore s a :: Type

  indexStore :: Store a -> Int -> a

  newStore :: Int -> ST s (MutableStore s a)

  writeStore :: MutableStore s a -> Int -> a -> ST s ()

  unsafeFreezeStore :: MutableStore s a -> ST s (Store a)

  copyStore :: MutableStore s a -> Int -> Store a -> Int -> Int -> ST s ()

  sizeOfStore :: Store a -> Int

newtype Vec (n :: Nat) a = Vec (Store a)

instance (Eq a, KnownNat n, Elem a) => Eq (Vec n a) where
  v1 == v2 = foldrPull (&&) True $ liftA2 (==) (delay v1) (delay v2)

instance (Show a, KnownNat n, Elem a) => Show (Vec n a) where
  show = show . toList

generate :: forall n a. (KnownNat n, Elem a) => (Fin n -> a) -> Vec n a
generate f = Vec $ runST $ do
  ms <- newStore @a size
  let go i
        | i < size = writeStore ms i (f $! Fin.unsafe i) >> go (i + 1)
        | otherwise = pure ()
  go 0
  unsafeFreezeStore @a ms
  where
    size = nat n
{-# INLINE generate #-}

replicate :: forall n a. (KnownNat n, Elem a) => a -> Vec n a
replicate = generate . const
{-# INLINE replicate #-}

index :: (Elem a) => Vec n a -> Fin n -> a
index (Vec s) i = indexStore s (Fin.toInt i)
{-# INLINE index #-}

set :: forall n a. (KnownNat n, Elem a) => Fin n -> a -> Vec n a -> Vec n a
set !i !x (Vec src) = Vec $ runST $ do
  ms <- newStore @a size
  copyStore @a ms 0 src 0 size
  writeStore ms (Fin.toInt i) x
  unsafeFreezeStore @a ms
  where
    size = nat n
{-# INLINE set #-}

modify :: forall n a. (KnownNat n, Elem a) => Fin n -> (a -> a) -> Vec n a -> Vec n a
modify !i f !v = let !x = index v i in set i (f x) v
{-# INLINE modify #-}

findIndex :: forall n a. (KnownNat n, Elem a) => (a -> Bool) -> Vec n a -> Maybe (Fin n)
findIndex p = ifoldr (\i x r -> if p x then Just i else r) Nothing
{-# INLINE findIndex #-}

coerce :: (Coercible a b, Store a ~ Store b) => Vec n a -> Vec n b
coerce (Vec s) = Vec s

map :: forall n a b. (KnownNat n, Elem a, Elem b) => (a -> b) -> Vec n a -> Vec n b
map f = force . fmap f . delay
{-# INLINE map #-}

rotate :: forall n a. (KnownNat n, Elem a) => Fin n -> Vec n a -> Vec n a
rotate fin (Vec s) = Vec $ runST $ do
  ms <- newStore @a size
  copyStore @a ms 0 s i (size - i)
  copyStore @a ms (size - i) s 0 i
  unsafeFreezeStore @a ms
  where
    size = nat n
    i = Fin.toInt fin
{-# INLINE rotate #-}

foldl' :: forall n a b. (KnownNat n, Elem a) => (b -> a -> b) -> b -> Vec n a -> b
foldl' f z = foldl'Pull f z . delay
{-# INLINE foldl' #-}

ifoldl' :: forall n a b. (KnownNat n, Elem a) => (Fin n -> b -> a -> b) -> b -> Vec n a -> b
ifoldl' f z = ifoldl'Pull f z . delay
{-# INLINE ifoldl' #-}

foldr :: forall n a b. (KnownNat n, Elem a) => (a -> b -> b) -> b -> Vec n a -> b
foldr f z = foldrPull f z . delay
{-# INLINE foldr #-}

ifoldr :: forall n a b. (KnownNat n, Elem a) => (Fin n -> a -> b -> b) -> b -> Vec n a -> b
ifoldr f z = ifoldrPull f z . delay
{-# INLINE ifoldr #-}

all :: forall n a. (KnownNat n, Elem a) => (a -> Bool) -> Vec n a -> Bool
all p = foldr (\x r -> p x && r) True
{-# INLINE all #-}

toList :: forall n a. (KnownNat n, Elem a) => Vec n a -> [a]
toList v = List.build (\cons nil -> foldrPull cons nil (delay v))
{-# INLINE toList #-}

fromList :: forall n a. (KnownNat n, Elem a) => [a] -> Maybe (Vec n a)
fromList xs = runST $ do
  ms <- newStore @a size
  let go !i = \case
        _ | i >= size -> Just . Vec <$> unsafeFreezeStore @a ms
        y : ys -> writeStore @a ms i y >> go (i + 1) ys
        [] -> pure Nothing
  go 0 xs
  where
    size = nat n

newtype Fill a = Fill (forall s. MutableStore s a -> ST s ())

generateA :: forall n a f. (KnownNat n, Elem a, Applicative f) => (Fin n -> f a) -> f (Vec n a)
generateA f =
  finish
    <$> Fin.foldr
      (\i -> liftA2 (step i) (f i))
      (pure $ Fill $ const $ pure ())
  where
    step !i !x (Fill k) = Fill @a $ \ms -> writeStore ms (Fin.toInt i) x >> k ms
    finish (Fill k) = Vec $ runST $ do
      ms <- newStore @a (nat n)
      k ms
      unsafeFreezeStore @a ms
{-# INLINE generateA #-}

mapM :: (KnownNat n, Elem a, Elem b, Applicative f) => (a -> f b) -> Vec n a -> f (Vec n b)
mapM f = traversePull f . delay
{-# INLINE mapM #-}

sequence :: (KnownNat n, Elem (f a), Elem a, Applicative f) => Vec n (f a) -> f (Vec n a)
sequence = traversePull id . delay
{-# INLINE sequence #-}

iforM_ :: forall n a f. (KnownNat n, Elem a, Applicative f) => Vec n a -> (Fin n -> a -> f ()) -> f ()
iforM_ (Vec s) f = go 0
  where
    size = nat n
    go i
      | i >= size = pure ()
      | otherwise = f (Fin.unsafe i) (indexStore s i) *> go (i + 1)
{-# INLINE iforM_ #-}

newtype Primed a = Primed {unprim :: a}
  deriving newtype (Eq, Ord, Show)

instance (Prim a) => Elem (Primed a) where
  type Store (Primed a) = ByteArray
  type MutableStore s (Primed a) = MutableByteArray s

  indexStore = \s -> Primed . indexByteArray s
  {-# INLINE indexStore #-}
  newStore = \n -> newByteArray (n * sizeOf (undefined :: a))
  {-# INLINE newStore #-}
  writeStore = \ms i -> writeByteArray ms i . unprim
  {-# INLINE writeStore #-}
  unsafeFreezeStore = unsafeFreezeByteArray
  {-# INLINE unsafeFreezeStore #-}
  copyStore = \mba i src j n ->
    let size = sizeOf (undefined :: a)
     in copyByteArray mba (i * size) src (j * size) (n * size)
  {-# INLINE copyStore #-}
  sizeOfStore = \ba -> sizeofByteArray ba `div` sizeOf (undefined :: a)
  {-# INLINE sizeOfStore #-}

newtype Boxed a = Boxed {unbox :: a}
  deriving newtype (Eq, Ord, Show)

instance Elem (Boxed a) where
  type Store (Boxed a) = SmallArray a
  type MutableStore s (Boxed a) = SmallMutableArray s a

  indexStore = \s -> Boxed . indexSmallArray s
  {-# INLINE indexStore #-}
  newStore = \n -> newSmallArray n (error "Data.Vec: newStore boxed")
  {-# INLINE newStore #-}
  writeStore = \sma !i (Boxed !x) -> writeSmallArray sma i x
  {-# INLINE writeStore #-}
  unsafeFreezeStore = unsafeFreezeSmallArray
  {-# INLINE unsafeFreezeStore #-}
  copyStore = copySmallArray
  {-# INLINE copyStore #-}
  sizeOfStore = sizeofSmallArray
  {-# INLINE sizeOfStore #-}

deriving via Primed Int instance Elem Int

deriving via Primed Int8 instance Elem Int8

deriving via Primed Int16 instance Elem Int16

deriving via Primed Int32 instance Elem Int32

deriving via Primed Int64 instance Elem Int64

deriving via Primed Word instance Elem Word

deriving via Primed Word8 instance Elem Word8

deriving via Primed Word16 instance Elem Word16

deriving via Primed Word32 instance Elem Word32

deriving via Primed Word64 instance Elem Word64

deriving via Primed Float instance Elem Float

deriving via Primed Double instance Elem Double

deriving via Primed Char instance Elem Char

deriving via Boxed [a] instance Elem [a]

deriving via Boxed (Maybe a) instance Elem (Maybe a)

deriving via Boxed Text instance Elem Text

newtype Pull (n :: Nat) a = Pull (Fin n -> a)
  deriving (Functor)

instance Applicative (Pull n) where
  pure x = Pull $ pure x
  Pull k <*> Pull g = Pull $ \i -> k i (g i)
  {-# INLINE (<*>) #-}

delay :: (Elem a) => Vec n a -> Pull n a
delay v = Pull (index v)
{-# INLINE [1] delay #-}

force :: (KnownNat n, Elem a) => Pull n a -> Vec n a
force (Pull f) = generate f
{-# INLINE [1] force #-}

{-# RULES "delay/force" forall p. delay (force p) = p #-}

{-# RULES "force/delay" forall v. force (delay v) = v #-}

foldl'Pull :: forall n a b. (KnownNat n) => (b -> a -> b) -> b -> Pull n a -> b
foldl'Pull f z (Pull g) = Fin.foldl' (\acc i -> f acc (g i)) z
{-# INLINE foldl'Pull #-}

ifoldl'Pull :: forall n a b. (KnownNat n) => (Fin n -> b -> a -> b) -> b -> Pull n a -> b
ifoldl'Pull f z (Pull g) = Fin.foldl' (\acc i -> f i acc (g i)) z
{-# INLINE ifoldl'Pull #-}

foldrPull :: forall n a b. (KnownNat n) => (a -> b -> b) -> b -> Pull n a -> b
foldrPull f z (Pull g) = Fin.foldr (f . g) z
{-# INLINE foldrPull #-}

ifoldrPull :: forall n a b. (KnownNat n) => (Fin n -> a -> b -> b) -> b -> Pull n a -> b
ifoldrPull f z (Pull g) = Fin.foldr (\i r -> f i (g i) r) z
{-# INLINE ifoldrPull #-}

traversePull :: (KnownNat n, Elem b, Applicative f) => (a -> f b) -> Pull n a -> f (Vec n b)
traversePull f (Pull g) = generateA (f . g)
{-# INLINE traversePull #-}
