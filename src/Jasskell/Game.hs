module Jasskell.Game where

import           Data.Finite
import           Data.Vector.Sized
import           Jasskell.Player
import           Jasskell.Round

data Game n = Game { players :: Vector n Player
                   , currentIndex :: Finite n
                   , currentRound :: Round n
                   , rounds :: [RoundFinished n]
                   }
