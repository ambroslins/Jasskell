module Jasskell.Variant
  ( Variant (..),
    Direction (..),
    next,
  )
where

import Jasskell.Card.Suit (Suit)

data Variant
  = Trump Suit
  | Direction Direction
  | Slalom Direction
  deriving (Eq, Show)

data Direction = TopDown | BottomUp
  deriving (Eq, Show)

next :: Variant -> Variant
next = \case
  Slalom TopDown -> Slalom BottomUp
  Slalom BottomUp -> Slalom TopDown
  variant -> variant
