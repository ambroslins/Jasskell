module Table where

import Data.Vector.Sized (Vector)
import Game (Game)
import User (User)

data Table n = Table
  { game :: Game n,
    users :: Vector n User
  }
  deriving (Show)