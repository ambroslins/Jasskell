module Jasskell.View.Declaring
  ( ViewDeclaring,
    hand,
    rounds,
    eldest,
    nominators,
    make,
  )
where

import Data.Finite (Finite)
import Data.Vector.Sized (Vector)
import Data.Vector.Sized qualified as Vector
import Jasskell.Card (Cards)
import Jasskell.Round (Round)
import Jasskell.Round qualified as Round
import Jasskell.Views qualified as Views

data ViewDeclaring n = ViewDeclaring
  { hand :: Cards,
    rounds :: [Round n],
    eldest :: Finite n,
    nominators :: NonEmpty (Finite n)
  }
  deriving (Eq, Show)

make ::
  KnownNat n =>
  Vector n Cards ->
  [Round n] ->
  Finite n ->
  NonEmpty (Finite n) ->
  Views.Views ViewDeclaring n
make hands rs e ns = Views.make $ \player ->
  ViewDeclaring
    { hand = Vector.index hands player,
      rounds = map (Round.rotate player) rs,
      eldest = e - player,
      nominators = ns
    }
