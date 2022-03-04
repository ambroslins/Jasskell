module Game where

import Round (Round, RoundFinished)

data Game n = Game
  { round :: Round n,
    previousRounds :: [RoundFinished n],
    settings :: Settings
  }
  deriving (Show)

data Settings = Settings
  deriving (Show)

data Error = Error
  deriving (Show)
