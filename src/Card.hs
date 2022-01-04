module Card where

import Suit

data Rank = Six | Seven | Eight | Nine | Ten | Under | Over | King | Ace
  deriving (Eq, Ord, Bounded, Enum, Show)

data Card = Card {suit :: Suit, rank :: Rank}
  deriving (Eq, Ord, Show)