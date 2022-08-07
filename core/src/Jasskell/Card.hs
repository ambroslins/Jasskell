module Jasskell.Card
  ( Card (..),
    Suit (..),
    Rank (..),
    Cards,
    deck,
    value,
    compare,
  )
where

import Data.Set qualified as Set
import Jasskell.Card.Suit (Suit (..))
import Jasskell.Variant (Direction (..), Variant (..))
import Prelude hiding (compare)

data Rank = Six | Seven | Eight | Nine | Ten | Under | Over | King | Ace
  deriving (Eq, Ord, Bounded, Enum, Show)

data Card = Card {suit :: Suit, rank :: Rank}
  deriving (Eq, Ord, Show)

type Cards = Set Card

deck :: Cards
deck = Set.fromList $ Card <$> universe <*> universe

value :: Variant -> Card -> Int
value variant card = case rank card of
  Six -> 0
  Seven -> 0
  Eight -> case variant of
    Trump _ -> 0
    Direction _ -> 8
    Slalom _ -> 8
  Nine -> if isTrump then 14 else 0
  Ten -> 10
  Under -> if isTrump then 20 else 2
  Over -> 3
  King -> 4
  Ace -> 11
  where
    isTrump = variant == Trump (suit card)

compare :: Variant -> Suit -> Card -> Card -> Ordering
compare variant lead = case variant of
  Trump trump ->
    comparing (== puur)
      <> comparing (== nell)
      <> comparing ((== trump) . suit)
      <> compareDirection TopDown
    where
      puur = Card trump Under
      nell = Card trump Nine
  Direction dir -> compareDirection dir
  Slalom dir -> compareDirection dir
  where
    compareDirection :: Direction -> Card -> Card -> Ordering
    compareDirection dir =
      comparing ((== lead) . suit)
        <> ( case dir of
               TopDown -> comparing rank
               BottomUp -> comparing (Down . rank)
           )
        <> comparing suit
