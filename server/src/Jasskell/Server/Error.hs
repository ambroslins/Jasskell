module Jasskell.Server.Error (Error (..)) where

import Jasskell.Server.GameState (BadMove)

data Error
  = WaitingForPlayers
  | GameOver
  | GameAlreadyStarted
  | BadMove BadMove
  | NotAPlayer
  | AlreadySeated
  | SeatAlreadyTaken
  deriving (Eq, Show)
