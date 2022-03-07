module Game where

import Round (Round)

newtype Game n = Game
  { rounds :: [Round n]
  }
  deriving (Show)
