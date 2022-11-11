module Jasskell.Server.PlayerView
  ( PlayerView,
    makeWaiting,
    makeActive,
    makeOver,
    encoder,
  )
where

import Data.Aeson (Key, Value)
import Data.Aeson.Combinators.Encode (Encoder, field, field')
import Data.Aeson.Combinators.Encode qualified as Encoder hiding (vector)
import Data.Vector.Sized (Vector)
import Data.Vector.Sized.Extra qualified as Vector
import Jasskell.Game (Game)
import Jasskell.Game qualified as Game
import Jasskell.Server.Encoder qualified as Encoder
import Jasskell.Server.GameState (GameView (..))
import Jasskell.Server.Seat (Seat (..))
import Jasskell.Server.Seat qualified as Seat
import Jasskell.View.Declaring qualified as View.Declaring
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
  Encoder.object' $ \playerView ->
    field' "seats" (Encoder.vector Seat.encoder) (seats playerView) :
    case phase playerView of
      Waiting -> encodePhase "waiting" () []
      Active (Playing view) ->
        encodePhase
          "playing"
          view
          [ field "hand" Encoder.cards View.Playing.hand,
            field "variant" Encoder.variant View.Playing.variant,
            field "leader" Encoder.finite View.Playing.leader,
            field
              "played-cards"
              (Encoder.vector (Encoder.nullable Encoder.card))
              View.Playing.playedCards,
            field "tricks" (Encoder.list Encoder.trick) View.Playing.tricks
          ]
      Active (Declaring view) ->
        encodePhase
          "declaring"
          view
          [ field "hand" Encoder.cards View.Declaring.hand,
            field "eldest" Encoder.finite View.Declaring.eldest,
            field
              "nominators"
              (Encoder.nonEmpty Encoder.finite)
              View.Declaring.nominators
          ]
      Over ga -> encodePhase "over" ga []
  where
    encodePhase :: Text -> a -> [a -> (Key, Value)] -> [(Key, Value)]
    encodePhase name view fields =
      field' "phase" Encoder.text name :
      map ($ view) fields
