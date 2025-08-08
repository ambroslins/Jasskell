module Jasskell.Player where

import Data.Text (Text)

data Player = Player
  { name :: Text,
    sendMessage :: Text -> IO ()
  }
