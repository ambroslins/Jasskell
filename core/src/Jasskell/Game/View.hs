module Jasskell.Game.View where

import Jasskell.Round (Round)
import Jasskell.Round.View qualified as Round (View)

data View n = MakeView
  { previousRounds :: [Round n],
    current :: SomeThing n
  }
  deriving (Eq, Show)

data SomeThing n
  = Playing (Round.View n)
  | Declaring
  deriving (Eq, Show)
