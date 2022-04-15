module Message where

import View qualified

data Message n
  = UpdateViewPlaying (View.Playing n)
  | UpdateViewDeclaring (View.Declaring n)
  deriving (Show)