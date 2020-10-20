module Jasskell.Trick
  ( Trick (..),
    TrickUnresolved,
    TrickResolved,
    playedCards,
    addCard,
    playedCard,
    newTrick,
    winner,
    currentIndex,
    points,
  )
where

import Data.Finite
import Data.Foldable
  ( maximumBy,
    toList,
  )
import Data.Function (on)
import Data.Vector.Sized
  ( Vector,
    index,
    indexed,
  )
import qualified Data.Vector.Sized as Vector
import GHC.TypeLits
import Jasskell.Card
import Jasskell.Variant

data TrickUnresolved n = TrickUnresolved
  { leadIndex :: Finite n,
    playedCards :: [Card]
  }

data TrickResolved n = TrickResolved (Finite n) (Vector n Card)

data Trick n
  = Unresolved (TrickUnresolved n)
  | Resolved (TrickResolved n)

addCard :: KnownNat n => Card -> TrickUnresolved n -> Trick n
addCard c (TrickUnresolved f cs) = case Vector.fromListN cs' of
  Just v -> Resolved $ TrickResolved f $ rotateN (- toInteger f) v
  Nothing -> Unresolved $ TrickUnresolved f cs'
  where
    cs' = cs ++ [c]

association :: KnownNat n => Trick n -> [(Finite n, Card)]
association (Unresolved t) = zip (iterate (+ 1) (leadIndex t)) $ playedCards t
association (Resolved (TrickResolved i v)) =
  toList $ rotateN (- toInteger i) $ Vector.indexed v

playedCard :: KnownNat n => Finite n -> Trick n -> Maybe Card
playedCard i = lookup i . association

newTrick :: Finite n -> TrickUnresolved n
newTrick f = TrickUnresolved f []

rotateN :: (KnownNat n) => Integer -> Vector n a -> Vector n a
rotateN n vec = Vector.generate (index vec . (+ modulo n))

winner :: Variant -> TrickResolved n -> Finite n
winner var (TrickResolved f v) =
  fst $ maximumBy (compareCard var (suit $ index v f) `on` snd) $ indexed v

currentIndex :: KnownNat n => TrickUnresolved n -> Finite n
currentIndex (TrickUnresolved i cs) = i + modulo (toInteger $ length cs)

points :: Variant -> TrickResolved n -> Int
points var (TrickResolved _ cs) = sum $ fmap (value var) cs
