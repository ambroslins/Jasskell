module Jasskell.Server.TableState
  ( TableState (..),
    Phase (..),
    findPlayer,
    playerViews,
    guestView,
  )
where

import Data.Finite (Finite)
import Data.Vector.Sized (Vector)
import Data.Vector.Sized qualified as Vector
import Jasskell.Dealer (Dealer)
import Jasskell.Game (Game)
import Jasskell.Server.Client (Client)
import Jasskell.Server.GameState (GameState)
import Jasskell.Server.GameState qualified as GameState
import Jasskell.Server.GuestView (GuestView)
import Jasskell.Server.GuestView qualified as GuestView
import Jasskell.Server.Player (Player)
import Jasskell.Server.Player qualified as Player
import Jasskell.Server.PlayerView (PlayerView)
import Jasskell.Server.PlayerView qualified as PlayerView
import Jasskell.Views (Views)
import System.Random (StdGen)

data TableState n = TableState
  { guests :: HashSet (Client n),
    players :: Vector n (Maybe (Player n)),
    stdGen :: StdGen,
    dealer :: Dealer n,
    phase :: Phase n
  }

data Phase n
  = Waiting
  | Playing (GameState n)
  | Over (Game n)

findPlayer :: Client n -> TableState n -> Maybe (Finite n)
findPlayer client =
  Vector.findIndex
    (maybe False (\p -> Player.client p == client))
    . players

playerViews :: KnownNat n => TableState n -> Views PlayerView n
playerViews tableState = case phase tableState of
  Waiting -> PlayerView.makeWaiting users
  Playing gameState -> PlayerView.makeActive users (GameState.views gameState)
  Over game -> PlayerView.makeOver users game
  where
    users = Vector.map (fmap Player.user) (players tableState)

guestView :: TableState n -> GuestView n
guestView tableState = case phase tableState of
  Waiting -> GuestView.makeWaiting users
  Playing _ -> GuestView.makeActive users
  Over _ -> GuestView.makeActive users
  where
    users = Vector.map (fmap Player.user) (players tableState)
