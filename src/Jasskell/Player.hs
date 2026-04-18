module Jasskell.Player
  ( Player (..),
  )
where

import Data.Text (Text)

newtype Player = Player {name :: Text}
  deriving (Eq, Show)
