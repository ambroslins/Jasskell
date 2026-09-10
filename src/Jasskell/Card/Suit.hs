{-# LANGUAGE TemplateHaskell #-}

module Jasskell.Card.Suit (Suit (..)) where

import Data.Aeson.TH (defaultOptions, deriveJSON)

data Suit = Bells | Hearts | Acorns | Leaves
  deriving (Eq, Ord, Enum, Bounded, Show)

$(deriveJSON defaultOptions ''Suit)
