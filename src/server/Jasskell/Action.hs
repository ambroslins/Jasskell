{-# LANGUAGE DeriveGeneric #-}

module Jasskell.Action where

import           Data.Aeson
import           Jasskell.Card
import           Jasskell.Variant
import           GHC.Generics

data Action = PlayCard Card
            | ChooseVariant Variant
            deriving (Show, Generic)

instance FromJSON Action
