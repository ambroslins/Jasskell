module Jasskell.Event where

import Data.Finite
import Jasskell.Action

data Event n = UserAction (Finite n) Action
