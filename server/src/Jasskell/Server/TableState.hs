module Jasskell.Server.TableState
  ( TableState (..),
    Seat (..),
    Phase (..),
    isEmpty,
    findClient,
    view,
  )
where

import Data.Finite (Finite)
import Data.HashMap.Strict qualified as HashMap
import Data.Vector.Sized (Vector)
import Data.Vector.Sized qualified as Vector
import Data.Vector.Sized.Extra qualified as Vector
import Jasskell.Dealer (Dealer)
import Jasskell.Game (Game)
import Jasskell.Game qualified as Game
import Jasskell.Server.Client (Client, ClientID)
import Jasskell.Server.GameState (GameState)
import Jasskell.Server.GameState qualified as GameState
import Jasskell.Server.User (User)
import Jasskell.Server.View (View)
import Jasskell.Server.View qualified as View
import Jasskell.Views qualified as Views
import System.Random (StdGen)

data TableState n = TableState
  { guests :: HashMap ClientID (Client n),
    seats :: Vector n (Seat n),
    stdGen :: StdGen,
    dealer :: Dealer n,
    phase :: Phase n
  }

data Seat n
  = Empty
  | Taken User ClientID (Client n)

data Phase n
  = Waiting
  | Playing (GameState n)
  | Over (Game n)

isEmpty :: Seat n -> Bool
isEmpty = \case
  Empty -> True
  _ -> False

findClient :: ClientID -> TableState n -> Maybe (Client n, Maybe (Finite n))
findClient clientID tableState =
  case findPlayer clientID tableState of
    Nothing -> (,Nothing) <$> findGuest clientID tableState
    Just (player, client) -> pure (client, pure player)

findPlayer :: ClientID -> TableState n -> Maybe (Finite n, Client n)
findPlayer clientID = asum . Vector.imap p . seats
  where
    p i = \case
      Empty -> Nothing
      Taken _ cid client -> guard (cid == clientID) $> (i, client)

findGuest :: ClientID -> TableState n -> Maybe (Client n)
findGuest clientID = HashMap.lookup clientID . guests

view :: KnownNat n => TableState n -> Finite n -> View n
view ts player =
  View.MakeView
    { View.seats =
        Vector.rotate (negate player) $ Vector.map mapSeat $ seats ts,
      View.phase = case phase ts of
        Waiting -> View.Waiting
        Playing gameState ->
          View.Started $ Views.pov (GameState.views gameState) player
        Over game -> View.Over $ Game.rotate player game
    }
  where
    mapSeat :: Seat n -> View.Seat
    mapSeat = \case
      Empty -> View.Empty
      Taken user _ _ -> View.Taken user
