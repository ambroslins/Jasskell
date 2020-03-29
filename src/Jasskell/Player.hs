module Jasskell.Player where

import           Data.Set                       ( delete )
import           Jasskell.Action
import           Jasskell.Card
import           Jasskell.Message

data Player = Player { name :: String
                     , cards :: Cards
                     , getAction :: IO Action
                     , putMessage :: Message -> IO ()
                     }

removeCard :: Card -> Player -> Player
removeCard c p = p { cards = delete c $ cards p }
