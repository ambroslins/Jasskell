module Jasskell.Player
    ( Player
    , cards
    , newPlayer
    , removeCard
    )
where

import           Data.Set                       ( delete )
import           Jasskell.Card

newtype Player = Player { cards :: Cards }

newPlayer :: Cards -> Player
newPlayer = Player

removeCard :: Card -> Player -> Player
removeCard c p = p { cards = delete c $ cards p }

