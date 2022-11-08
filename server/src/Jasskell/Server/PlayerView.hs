module Jasskell.Server.PlayerView
  ( PlayerView,
    makeWaiting,
    makeActive,
    makeOver,
    encoder,
  )
where

import Data.Aeson.Combinators.Encode (Encoder, field)
import Data.Aeson.Combinators.Encode qualified as Encoder hiding (vector)
import Data.Vector.Sized (Vector)
import Data.Vector.Sized.Extra qualified as Vector
import Jasskell.Game (Game)
import Jasskell.Game qualified as Game
import Jasskell.Server.Encoder (Tagged (..))
import Jasskell.Server.Encoder qualified as Encoder
import Jasskell.Server.GameState (GameView (..))
import Jasskell.Server.Seat (Seat (..))
import Jasskell.Server.Seat qualified as Seat
import Jasskell.View.Declaring (ViewDeclaring)
import Jasskell.View.Declaring qualified as View.Declaring
import Jasskell.View.Playing (ViewPlaying)
import Jasskell.View.Playing qualified as View.Playing
import Jasskell.Views (Views)
import Jasskell.Views qualified as Views

data PlayerView n = MakePlayerView
  { seats :: Vector n Seat,
    phase :: Phase n
  }
  deriving (Show)

data Phase n
  = Waiting
  | Active (GameView n)
  | Over (Game n)
  deriving (Show)

makeViews ::
  KnownNat n =>
  (view n -> Phase n) ->
  Vector n Seat ->
  Views view n ->
  Views PlayerView n
makeViews f ss = Views.map $ \i view ->
  MakePlayerView
    { seats = Vector.rotate i ss,
      phase = f view
    }

makeWaiting ::
  KnownNat n =>
  Vector n Seat ->
  Views PlayerView n
makeWaiting ss = makeViews id ss (Views.make (const Waiting))

makeActive ::
  KnownNat n =>
  Vector n Seat ->
  Views GameView n ->
  Views PlayerView n
makeActive = makeViews Active

makeOver ::
  KnownNat n =>
  Vector n Seat ->
  Game n ->
  Views PlayerView n
makeOver ss game = makeViews Over ss (Views.make $ \i -> Game.rotate i game)

encoder :: forall n. (KnownNat n) => Encoder (PlayerView n)
encoder =
  Encoder.object
    [ field "seats" (Encoder.vector Seat.encoder) seats,
      field "phase" phaseEncoder phase
    ]
  where
    phaseEncoder :: Encoder (Phase n)
    phaseEncoder = Encoder.tagged $ \case
      Waiting -> Tagged "waiting" Encoder.unit ()
      Active (Playing view) -> Tagged "playing" playingEncoder view
      Active (Declaring view) -> Tagged "declaring" declaringEncoder view
      Over game -> Tagged "over" Encoder.game game

    playingEncoder :: Encoder (ViewPlaying n)
    playingEncoder =
      Encoder.object
        [ field "hand" Encoder.cards View.Playing.hand,
          field "variant" Encoder.variant View.Playing.variant,
          field "leader" Encoder.finite View.Playing.leader,
          field
            "played-cards"
            (Encoder.vector (Encoder.nullable Encoder.card))
            View.Playing.playedCards,
          field "tricks" (Encoder.list Encoder.trick) View.Playing.tricks
        ]

    declaringEncoder :: Encoder (ViewDeclaring n)
    declaringEncoder =
      Encoder.object
        [ field "hand" Encoder.cards View.Declaring.hand,
          field "eldest" Encoder.finite View.Declaring.eldest,
          field
            "nominators"
            (contramap toList (Encoder.list Encoder.finite))
            View.Declaring.nominators
        ]
