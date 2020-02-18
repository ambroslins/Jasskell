module Jasskell.Variant where

import Jasskell.Card.Suit

data Variant = Trump Suit | Direction Direction | Slalom Direction
    deriving (Eq, Show)

data Direction = BottomUp | TopDown deriving (Eq, Show)
