module Jasskell.Card where

data Card = Card { suit :: Suit, rank :: Rank } deriving (Eq, Ord, Show)
