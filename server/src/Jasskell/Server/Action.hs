module Jasskell.Server.Action where

import Jasskell.Server.GameState (Move)

data Action
  = Move Move
  | StartGame
  deriving (Eq, Show)