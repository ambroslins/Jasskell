module Jasskell.Player where

import           Data.Set                       ( delete )
import           Jasskell.Card

newtype Player = Player { cards :: Cards }

removeCard :: Card -> Player -> Player
removeCard c p = p { cards = delete c $ cards p }

