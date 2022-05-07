module Jasskell.Card.Suit (Suit (..)) where

data Suit = Bells | Hearts | Acorns | Leaves
  deriving (Eq, Ord, Bounded, Enum, Show)