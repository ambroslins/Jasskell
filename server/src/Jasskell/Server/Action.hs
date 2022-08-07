module Jasskell.Server.Action where

import Jasskell.Card (Card)
import Jasskell.Variant (Variant)

data Action
  = PlayCard Card
  | DeclareVariant Variant
  deriving (Eq, Show)