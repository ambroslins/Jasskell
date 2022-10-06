module Jasskell.Server.TableState
  ( TableState (..),
    Seat (..),
    Phase (..),
    isEmptySeat,
    findPlayer,
    viewPlayer,
    viewGuest,
  )
where

import Data.Finite (Finite)
import Data.Set qualified as Set
import Data.Vector.Sized (Vector)
import Data.Vector.Sized qualified as Vector
import Data.Vector.Sized.Extra qualified as Vector
import Jasskell.Dealer (Dealer)
import Jasskell.Game (Game)
import Jasskell.Game qualified as Game
import Jasskell.Server.Client (Client)
import Jasskell.Server.GameState (GameState)
import Jasskell.Server.GameState qualified as GameState
import Jasskell.Server.User (User)
import Jasskell.Server.View (View)
import Jasskell.Server.View qualified as View
import Jasskell.View.Declaring qualified as View.Declaring
import Jasskell.View.Playing qualified as View.Playing
import Jasskell.Views qualified as Views
import System.Random (StdGen)

data TableState n = TableState
  { guests :: HashSet (Client n),
    seats :: Vector n (Seat n),
    stdGen :: StdGen,
    dealer :: Dealer n,
    phase :: Phase n
  }

data Seat n
  = Empty
  | Taken User (Client n)

data Phase n
  = Waiting
  | Playing (GameState n)
  | Over (Game n)

isEmptySeat :: Seat n -> Bool
isEmptySeat = \case
  Empty -> True
  _ -> False

findPlayer :: Client n -> TableState n -> Maybe (Finite n)
findPlayer client =
  Vector.findIndex
    (\case Empty -> False; Taken _ c -> c == client)
    . seats

viewPlayer :: KnownNat n => TableState n -> Finite n -> View n
viewPlayer ts player =
  View.MakeView
    { View.seats = Vector.rotate player . mapSeats $ seats ts,
      View.phase = case phase ts of
        Waiting -> View.Waiting
        Playing gameState -> Views.pov (GameState.viewPhase gameState) player
        Over game -> View.Over $ Game.rotate player game
    }

mapSeats :: Vector n (Seat n) -> Vector n View.Seat
mapSeats = Vector.map $ \case
  Empty -> View.Empty
  Taken user _ -> View.Taken user

viewGuest :: KnownNat n => TableState n -> View n
viewGuest ts = playerView {View.phase = phase}
  where
    playerView = viewPlayer ts 0
    phase = case View.phase playerView of
      View.Playing v -> View.Playing v {View.Playing.hand = Set.empty}
      View.Declaring v -> View.Declaring v {View.Declaring.hand = Set.empty}
      p -> p