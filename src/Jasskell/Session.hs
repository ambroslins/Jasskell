module Jasskell.Session where

import Control.Concurrent.STM.TVar (TVar)
import Data.Vector4 (Vector4)
import Jasskell.GameState (GameState)

data Session = Session
  { gameState :: !(TVar GameState),
    seats :: !(TVar (Vector4 Player))
  }

data Seat
  = Empty
  | Taken Player
  | Disconnected String

data Player = Player
  { name :: String,
    sendMessage :: String -> IO ()
  }
