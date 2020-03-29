module Jasskell.Player where

import           Jasskell.Action
import           Jasskell.Card
import           Jasskell.Message

data Player = Player { name :: String
                     , cards :: Cards
                     , getAction :: IO Action
                     , putMessage :: Message -> IO ()
                     }
