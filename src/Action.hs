module Action where

import Card (Card)
import Variant (Variant)

data Action
  = PlayCard Card
  | ChooseVariant Variant
  deriving (Show)