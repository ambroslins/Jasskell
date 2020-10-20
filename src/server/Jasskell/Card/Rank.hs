{-# LANGUAGE DeriveGeneric #-}

module Jasskell.Card.Rank where

import Data.Aeson
import GHC.Generics

data Rank = Six | Seven | Eight | Nine | Ten | Under | Over | King | Ace
  deriving (Eq, Ord, Bounded, Enum, Show, Read, Generic)

instance ToJSON Rank

instance FromJSON Rank
