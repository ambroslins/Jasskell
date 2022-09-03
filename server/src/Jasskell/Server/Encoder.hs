module Jasskell.Server.Encoder where

import Data.Aeson.Combinators.Encode
  ( Encoder,
    field,
    object,
    text,
  )
import Data.Aeson.Combinators.Encode qualified as Encoder
import Data.Finite (Finite, getFinite)
import Data.Vector.Sized (Vector)
import Data.Vector.Sized qualified as Vector
import Jasskell.Card (Card, Cards, Rank, Suit)
import Jasskell.Card qualified as Card
import Jasskell.Game (Game)
import Jasskell.Game qualified as Game
import Jasskell.Round (Round)
import Jasskell.Round qualified as Round
import Jasskell.Server.GameState qualified as GameState
import Jasskell.Server.User (User)
import Jasskell.Server.User qualified as User
import Jasskell.Server.View (Phase (..), Seat (..))
import Jasskell.Trick (Trick)
import Jasskell.Trick qualified as Trick
import Jasskell.Variant (Direction, Variant (..))
import Jasskell.View.Declaring qualified as View.Declaring
import Jasskell.View.Playing qualified as View.Playing
import Prelude hiding (round)

set :: Encoder a -> Encoder (Set a)
set = contramap toList . Encoder.list

vector :: Encoder a -> Encoder (Vector n a)
vector = contramap Vector.fromSized . Encoder.vector

suit :: Encoder Suit
suit = contramap show text

rank :: Encoder Rank
rank = contramap show text

card :: Encoder Card
card =
  object
    [ field "suit" suit Card.suit,
      field "rank" rank Card.rank
    ]

cards :: Encoder Cards
cards = set card

finite :: Encoder (Finite n)
finite = contramap getFinite Encoder.integer

direction :: Encoder Direction
direction = contramap show Encoder.text

data Tagged = forall a. Tagged Encoder.Key (Encoder a) a

tagged :: (a -> Tagged) -> Encoder a
tagged f = Encoder.object' $ \a ->
  case f a of
    (Tagged name encoder value) -> [Encoder.field' name encoder value]

variant :: Encoder Variant
variant = tagged $ \case
  Trump s -> Tagged "trump" suit s
  Direction d -> Tagged "direction" direction d
  Slalom d -> Tagged "slalom" direction d

user :: Encoder User
user =
  object
    [ field "name" Encoder.text User.name
    ]

seat :: Encoder Seat
seat = tagged $ \case
  Empty -> Tagged "empty" Encoder.unit ()
  Taken u -> Tagged "taken" user u

trick :: Encoder (Trick n)
trick =
  object
    [ field "variant" variant Trick.variant,
      field "leader" finite Trick.leader,
      field "rounds" (vector card) Trick.cards
    ]

round :: Encoder (Round n)
round = contramap Round.tricks (vector trick)

game :: Encoder (Game n)
game = contramap Game.rounds (Encoder.list round)

viewDeclaring :: Encoder (View.Declaring.Declaring n)
viewDeclaring =
  object
    [ field "hand" (set card) View.Declaring.hand,
      field "rounds" (Encoder.list round) View.Declaring.rounds,
      field "eldest" finite View.Declaring.eldest,
      field "nominators" (contramap toList $ Encoder.list finite) View.Declaring.nominators
    ]

viewPlaying :: Encoder (View.Playing.Playing n)
viewPlaying =
  object
    [ field "hand" (set card) View.Playing.hand,
      field "rounds" (Encoder.list round) View.Playing.rounds,
      field "tricks" (Encoder.list trick) View.Playing.tricks,
      field "variant" variant View.Playing.variant,
      field "leader" finite View.Playing.leader,
      field "cards" (Encoder.list card) View.Playing.cards
    ]

viewGameState :: Encoder (GameState.View n)
viewGameState = tagged $ \case
  GameState.Playing playing -> Tagged "playing" viewPlaying playing
  GameState.Declaring declaring -> Tagged "declaring" viewDeclaring declaring

phase :: Encoder (Phase n)
phase = tagged $ \case
  Waiting -> Tagged "waiting" Encoder.unit ()
  Started v -> Tagged "started" viewGameState v
  Over g -> Tagged "over" game g
