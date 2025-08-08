module Jasskell.Variant (Variant (..), Direction (..), next) where

import Jasskell.Card.Suit (Suit)

data Variant
  = Trump !Suit
  | Direction !Direction
  | Slalom !Direction
  deriving (Eq, Show)

data Direction = BottomUp | TopDown
  deriving (Eq, Show)

next :: Variant -> Variant
next = \case
  Slalom BottomUp -> Slalom TopDown
  Slalom TopDown -> Slalom BottomUp
  v -> v
