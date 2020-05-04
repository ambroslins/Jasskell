{-# LANGUAGE DeriveGeneric #-}

module Jasskell.Action where

import           Data.Aeson
import           Jasskell.Card
import           Jasskell.GameID
import           Jasskell.Variant
import           GHC.Generics

data Action = PlayCard Card
            | ChooseVariant Variant
            | Join GameID String
            deriving (Show, Generic)

instance FromJSON Action
