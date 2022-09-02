module Jasskell.Server.View
  ( View (..),
    Seat (..),
    Phase (..),
  )
where

import Data.Vector.Sized (Vector)
import Jasskell.Game (Game)
import Jasskell.Server.GameState qualified as GameState
import Jasskell.Server.User (User)

data View n = MakeView
  { seats :: Vector n Seat,
    phase :: Phase n
  }
  deriving (Show)

data Seat
  = Empty
  | Taken User
  deriving (Eq, Show)

data Phase n
  = Waiting
  | Playing (GameState.View n)
  | Over (Game n)
  deriving (Show)
