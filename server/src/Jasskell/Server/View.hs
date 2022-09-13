module Jasskell.Server.View
  ( View (..),
    Seat (..),
    Phase (..),
  )
where

import Data.Vector.Sized (Vector)
import Jasskell.Game (Game)
import Jasskell.Server.User (User)
import Jasskell.View.Declaring (ViewDeclaring)
import Jasskell.View.Playing (ViewPlaying)

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
  | Playing (ViewPlaying n)
  | Declaring (ViewDeclaring n)
  | Over (Game n)
  deriving (Show)
