module Jasskell.Player
  ( Player,
    name,
  )
where

import Data.Text (Text)

newtype Player = Player {name :: Text}
  deriving (Eq, Show)
