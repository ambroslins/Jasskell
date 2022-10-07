module Jasskell.View.Declaring
  ( ViewDeclaring,
    makeViews,
    hand,
    rounds,
    eldest,
    nominators,
  )
where

import Data.Finite (Finite)
import Data.Vector.Sized (Vector)
import Data.Vector.Sized qualified as Vector
import Jasskell.Card (Cards)
import Jasskell.Round (Round)
import Jasskell.Round qualified as Round
import Jasskell.Views (Views)
import Jasskell.Views qualified as Views

data ViewDeclaring n = MakeViewDeclaring
  { hand :: Cards,
    rounds :: [Round n],
    eldest :: Finite n,
    nominators :: NonEmpty (Finite n)
  }
  deriving (Eq, Show)

makeViews ::
  KnownNat n =>
  Vector n Cards ->
  [Round n] ->
  Finite n ->
  NonEmpty (Finite n) ->
  Views ViewDeclaring n
makeViews hands rs e ns = Views.make $ \i ->
  MakeViewDeclaring
    { hand = Vector.index hands i,
      rounds = map (Round.rotate i) rs,
      eldest = e - i,
      nominators = subtract i <$> ns
    }
