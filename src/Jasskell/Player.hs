module Jasskell.Player where

import           Jasskell.Action
import           Jasskell.Card
import           Jasskell.Message

data Player = Player { cards :: Cards
                     , getAction :: IO Action
                     , putMessage :: Message -> IO ()
                     }
