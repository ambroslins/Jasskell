module Jasskell.Card where

import           Jasskell.Card.Suit
import           Jasskell.Card.Rank

data Card = Card { suit :: Suit, rank :: Rank } deriving (Eq, Ord, Show)
