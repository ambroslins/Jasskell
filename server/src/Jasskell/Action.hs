module Jasskell.Action where

import           Jasskell.Card
import           Jasskell.Variant

data Action = PlayCard Card
            | ChooseVariant Variant
