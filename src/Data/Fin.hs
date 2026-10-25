{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Fin
  ( Fin,
    toInt,
    nat,
    make,
    modulo,
    foldl',
    foldr,
    fins,
    for_,
    unsafe,
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..))
import GHC.Exts (proxy#)
import GHC.List qualified as List
import GHC.TypeLits (KnownNat, Nat, natVal', type (<=))
import Prelude hiding (foldl', foldr)

newtype Fin (n :: Nat) = Fin Int
  deriving newtype (Eq, Ord, Show, ToJSON)

instance (KnownNat n) => Num (Fin n) where
  Fin a + Fin b = Fin $ (a + b) `rem` nat n
  Fin a - Fin b = Fin $ (a - b) `mod` nat n
  Fin a * Fin b = Fin $ (a * b) `rem` nat n
  negate (Fin i) = Fin $ negate i `mod` nat n
  abs = id
  signum (Fin i) = if i == 0 then Fin 0 else Fin 1
  fromInteger i = Fin $ fromIntegral i `mod` nat n

instance (KnownNat n, 1 <= n) => Bounded (Fin n) where
  minBound = Fin 0
  maxBound = Fin $ nat n - 1

instance (KnownNat n) => FromJSON (Fin n) where
  parseJSON v =
    parseJSON v
      >>= maybe (fail "finite integer out of range") pure . make

toInt :: Fin n -> Int
toInt (Fin i) = i

unsafe :: Int -> Fin n
unsafe = Fin

nat :: forall n -> (KnownNat n) => Int
nat n = fromIntegral (natVal' (proxy# @n))
{-# INLINE nat #-}

make :: forall n. (KnownNat n) => Int -> Maybe (Fin n)
make i
  | i >= 0 && i < nat n = Just $! unsafe i
  | otherwise = Nothing
{-# INLINE make #-}

modulo :: forall n. (KnownNat n) => Int -> Fin n
modulo i = unsafe $ i `mod` nat n
{-# INLINE modulo #-}

foldl' :: forall n a. (KnownNat n) => (a -> Fin n -> a) -> a -> a
foldl' f = go 0
  where
    m = nat n
    go !i !acc
      | i >= m = acc
      | otherwise = go (i + 1) (f acc (unsafe i))
{-# INLINE foldl' #-}

foldr :: forall n a. (KnownNat n) => (Fin n -> a -> a) -> a -> a
foldr f z = go 0
  where
    m = nat n
    go !i
      | i >= m = z
      | otherwise = f (unsafe i) (go (i + 1))
{-# INLINE foldr #-}

fins :: forall n. (KnownNat n) => [Fin n]
fins = List.build foldr
{-# INLINE fins #-}

for_ :: (KnownNat n, Applicative f) => (Fin n -> f ()) -> f ()
for_ k = foldr (\i r -> k i *> r) (pure ())
{-# INLINE for_ #-}
