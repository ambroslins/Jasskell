module Message where

import Card.Valid (Reason)
import View (Phase (..), View)

data Message n
  = UpdateViewPlaying (View 'Playing n)
  | UpdateViewDeclaring (View 'Declaring n)
  deriving (Show)