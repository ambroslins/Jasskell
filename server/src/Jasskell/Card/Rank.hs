module Jasskell.Card.Rank where

data Rank = Six | Seven | Eight | Nine | Ten | Under | Over | King | Ace
    deriving(Eq, Ord, Bounded, Enum, Show, Read)
