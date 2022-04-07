module Game where

import Round (Round)
import Prelude hiding (round)

newtype Game n = Game {rounds :: NonEmpty (Round n)}
  deriving (Show)
