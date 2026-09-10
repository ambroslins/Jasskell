module Data.Vector4 where

import Data.Aeson (FromJSON, ToJSON)
import Data.Bits ((.&.))
import Data.Coerce (coerce)
import Prelude hiding (replicate)

data Vector4 a = Vector4 !a !a !a !a
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Applicative Vector4 where
  pure = replicate
  Vector4 f0 f1 f2 f3 <*> Vector4 x0 x1 x2 x3 =
    Vector4 (f0 x0) (f1 x1) (f2 x2) (f3 x3)
  liftA2 f (Vector4 x0 x1 x2 x3) (Vector4 y0 y1 y2 y3) =
    Vector4 (f x0 y0) (f x1 y1) (f x2 y2) (f x3 y3)

newtype Index4 = Index4 Int
  deriving newtype (Eq, Ord, Show, ToJSON, FromJSON) -- TODO: handle invariant in json parser

instance Enum Index4 where
  toEnum = Index4 . (.&. 3)
  fromEnum = coerce
  succ (Index4 i) = Index4 $ (i + 1) .&. 3
  pred (Index4 i) = Index4 $ (i - 1) .&. 3

instance Bounded Index4 where
  minBound = Index4 0
  maxBound = Index4 3

instance Num Index4 where
  Index4 i + Index4 j = Index4 $ (i + j) .&. 3
  Index4 i * Index4 j = Index4 $ (i * j) .&. 3
  Index4 i - Index4 j = Index4 $ (i - j) `mod` 4
  abs = id
  signum (Index4 i) = Index4 $ if i == 0 then 0 else 1
  negate (Index4 i) = Index4 $ negate i `mod` 4
  fromInteger = Index4 . (`mod` 4) . fromInteger

make :: a -> a -> a -> a -> Vector4 a
make = Vector4

replicate :: a -> Vector4 a
replicate x = Vector4 x x x x

index :: Index4 -> Vector4 a -> a
index (Index4 i) (Vector4 x0 x1 x2 x3) = case i of
  0 -> x0
  1 -> x1
  2 -> x2
  3 -> x3
  _ -> error "index: index out of bounds"

set :: Index4 -> a -> Vector4 a -> Vector4 a
set (Index4 i) x (Vector4 x0 x1 x2 x3) = case i of
  0 -> Vector4 x x1 x2 x3
  1 -> Vector4 x0 x x2 x3
  2 -> Vector4 x0 x1 x x3
  3 -> Vector4 x0 x1 x2 x
  _ -> error "set: index out of bounds"

modify :: Index4 -> (a -> a) -> Vector4 a -> Vector4 a
modify (Index4 i) f (Vector4 x0 x1 x2 x3) = case i of
  0 -> Vector4 (f x0) x1 x2 x3
  1 -> Vector4 x0 (f x1) x2 x3
  2 -> Vector4 x0 x1 (f x2) x3
  3 -> Vector4 x0 x1 x2 (f x3)
  _ -> error "modify: index out of bounds"

rotate :: Index4 -> Vector4 a -> Vector4 a
rotate (Index4 i) v@(Vector4 x0 x1 x2 x3) = case i of
  0 -> v
  1 -> Vector4 x1 x2 x3 x0
  2 -> Vector4 x2 x3 x0 x1
  3 -> Vector4 x3 x0 x1 x2
  _ -> error "rotate: index out of bounds"

findIndex :: (a -> Bool) -> Vector4 a -> Maybe Index4
findIndex p (Vector4 x0 x1 x2 x3)
  | p x0 = Just 0
  | p x1 = Just 1
  | p x2 = Just 2
  | p x3 = Just 3
  | otherwise = Nothing

maxIndexBy :: (a -> a -> Ordering) -> Vector4 a -> Index4
maxIndexBy cmp (Vector4 x0 x1 x2 x3) =
  fst $ (0, x0) `maxBy` (1, x1) `maxBy` (2, x2) `maxBy` (3, x3)
  where
    maxBy (ix, x) (iy, y) = case cmp x y of
      LT -> (ix, x)
      _ -> (iy, y)

imap :: (Index4 -> a -> b) -> Vector4 a -> Vector4 b
imap f (Vector4 x0 x1 x2 x3) = Vector4 (f 0 x0) (f 1 x1) (f 2 x2) (f 3 x3)

iforM_ :: (Monad m) => Vector4 a -> (Index4 -> a -> m b) -> m ()
iforM_ v f = sequence_ $ imap f v
