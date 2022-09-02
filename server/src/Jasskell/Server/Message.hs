module Jasskell.Server.Message where

import Data.Finite (Finite)
import Jasskell.View.Declaring (Declaring)
import Jasskell.View.Playing (Playing)

data Message n
  = UpdateViewPlaying (Playing n)
  | UpdateViewDeclaring (Declaring n)
  | UserJoined Text (Finite n)
  deriving (Show)