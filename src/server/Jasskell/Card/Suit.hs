{-# LANGUAGE DeriveGeneric #-}

module Jasskell.Card.Suit where

import Data.Aeson
import GHC.Generics

data Suit = Bells | Hearts | Acorns | Leaves
  deriving (Eq, Ord, Bounded, Enum, Show, Read, Generic)

instance ToJSON Suit

instance FromJSON Suit
