module Jasskell.Server.Error (Error (..)) where

import Jasskell.Server.GameState (BadMove)

data Error
  = WaitingForPlayers
  | GameOver
  | GameAlreadyStarted
  | BadMove BadMove
  deriving (Eq, Show)
