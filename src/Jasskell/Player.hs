module Jasskell.Player where

import           Jasskell.Action
import           Jasskell.Card

data Player = Player { cards :: Cards
                     , getAction :: IO Action
                     }
