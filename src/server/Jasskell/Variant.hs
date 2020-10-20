{-# LANGUAGE DeriveGeneric #-}

module Jasskell.Variant where

import Data.Aeson
import GHC.Generics
import Jasskell.Card.Suit

data Variant = Trump Suit | Direction Direction | Slalom Direction
  deriving (Eq, Show, Generic)

instance ToJSON Variant

instance FromJSON Variant

nextVariant :: Variant -> Variant
nextVariant (Slalom TopDown) = Slalom BottomUp
nextVariant (Slalom BottomUp) = Slalom TopDown
nextVariant v = v

data Direction = BottomUp | TopDown
  deriving (Eq, Show, Generic)

instance ToJSON Direction

instance FromJSON Direction
