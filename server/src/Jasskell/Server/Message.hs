module Jasskell.Server.Message where

import Data.Finite (Finite)
import Jasskell.Game (Game)
import Jasskell.View qualified as View

data Message n
  = UpdateView (View.SomeView n)
  | GameOver (Game n)
  | UserJoined Text (Finite n)
  deriving (Show)