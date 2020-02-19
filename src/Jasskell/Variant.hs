module Jasskell.Variant where

import           Jasskell.Card.Suit

data Variant = Trump Suit | Direction Direction | Slalom Direction
    deriving (Eq, Show)

data Direction = BottomUp | TopDown deriving (Eq, Show)

nextVariant :: Variant -> Variant
nextVariant (Slalom TopDown ) = Slalom BottomUp
nextVariant (Slalom BottomUp) = Slalom TopDown
nextVariant v                 = v
