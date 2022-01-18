module Game where

import Round (Round)
import Round qualified
import Prelude hiding (round)

data Game n = Game
  { round :: Round n,
    previousRounds :: [Round.Record n],
    settings :: Settings
  }
  deriving (Show)

data Settings = Settings
  deriving (Show)
