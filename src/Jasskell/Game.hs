module Jasskell.Game where

import           Data.Finite
import           Data.Vector.Sized
import           Jasskell.Player

data Game n = Game { players :: Vector n Player
                   , currentIndex :: Finite n }
