{-# LANGUAGE DeriveGeneric #-}

module Jasskell.Action where

import Data.Aeson
import GHC.Generics
import Jasskell.Card
import Jasskell.Variant

data Action
  = PlayCard Card
  | ChooseVariant Variant
  deriving (Show, Generic)

instance FromJSON Action
