{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Fin
  ( Fin,
    toInt,
    nat,
    make,
    modulo,
    parity,
    foldl',
    foldr,
    fins,
    for_,
    unsafe,
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Bits ((.&.))
import Data.Coerce (coerce)
import Data.Primitive (Prim)
import GHC.Exts (proxy#)
import GHC.List qualified as List
import GHC.TypeLits (KnownNat, Nat, natVal', type (<=))
import Prelude hiding (foldl', foldr)

newtype Fin (n :: Nat) = Fin Int
  deriving newtype (Eq, Ord, Show, ToJSON, Prim)

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

instance (KnownNat n) => Enum (Fin n) where
  fromEnum (Fin i) = i
  toEnum i
    | 0 <= i && i < nat n = Fin i
    | otherwise = error $ "Fin.toEnum: " <> show i <> " out of range for Fin " <> show (nat n)
  succ (Fin i)
    | let j = i + 1, j < nat n = Fin j
    | otherwise = error "Fin.succ: maxBound"
  pred (Fin i)
    | i > 0 = Fin i
    | otherwise = error "Fin.pred: minBound"
  enumFrom (Fin i) = coerce [i .. nat n - 1]
  enumFromTo (Fin i) (Fin j) = coerce [i .. j]
  enumFromThen (Fin i) (Fin j) =
    coerce [i, j .. if j >= i then nat n - 1 else 0]
  enumFromThenTo (Fin i) (Fin j) (Fin k) =
    coerce [i, j .. k]

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

parity :: forall n. Fin n -> Fin 2
parity (Fin i) = Fin $ i .&. 1

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
