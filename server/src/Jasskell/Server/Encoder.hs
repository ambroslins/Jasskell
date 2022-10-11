module Jasskell.Server.Encoder
  ( Tagged (..),
    tagged,
    set,
    vector,
    nullable,
    finite,
    suit,
    rank,
    card,
    cards,
    variant,
    trick,
    round,
    game,
  )
where

import Data.Aeson (Value (Null))
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
import Jasskell.Trick (Trick)
import Jasskell.Trick qualified as Trick
import Jasskell.Variant (Direction, Variant (..))
import Prelude hiding (round)

set :: Encoder a -> Encoder (Set a)
set = contramap toList . Encoder.list

vector :: Encoder a -> Encoder (Vector n a)
vector = contramap Vector.fromSized . Encoder.vector

nullable :: Encoder a -> Encoder (Maybe a)
nullable e = Encoder.Encoder $ maybe Null (Encoder.run e)

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
