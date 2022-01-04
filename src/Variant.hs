module Variant
  ( Variant (..),
    Direction (..),
    next,
  )
where

import Suit

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
