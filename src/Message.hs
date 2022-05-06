module Message where

import Game (Game)
import View qualified

data Message n
  = UpdateView (View.SomeView n)
  | GameOver (Game n)
  deriving (Show)