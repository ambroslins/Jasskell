module Jasskell.Server.Message where

import Jasskell.Server.Error (Error)
import Jasskell.Server.View (View)

data Message n
  = UpdatePlayerView (View n)
  | UpdateGuestView (View n)
  | Error Error
  deriving (Show)