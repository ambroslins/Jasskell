module Action where

import Card (Card)
import Variant (Variant)

data Action
  = PlayCard Card
  | DeclareVariant Variant
  deriving (Eq, Show)