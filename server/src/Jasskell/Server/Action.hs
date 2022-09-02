module Jasskell.Server.Action where

import Jasskell.Server.GameState (Move)

newtype Action
  = Move Move
  deriving (Eq, Show)