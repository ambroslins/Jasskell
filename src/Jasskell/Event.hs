module Jasskell.Event where

import           Data.Finite
import           Jasskell.Action

data Event n = PlayerAction (Finite n) Action
