module Message where

import Data.Finite (Finite)
import Game (Game)
import View qualified

data Message n
  = UpdateView (View.SomeView n)
  | GameOver (Game n)
  | UserJoined Text (Finite n)
  deriving (Show)