module Jasskell.View.Declaring where

import Data.Finite (Finite)
import Data.Vector.Sized (Vector)
import Data.Vector.Sized qualified as Vector
import Jasskell.Card (Cards)
import Jasskell.Round (Round)
import Jasskell.Round qualified as Round
import Jasskell.Views qualified as Views

data Declaring n = Declaring
  { hand :: Cards,
    rounds :: [Round n],
    eldest :: Finite n
  }
  deriving (Eq, Show)

make ::
  KnownNat n =>
  Vector n Cards ->
  [Round n] ->
  Finite n ->
  Views.Views Declaring n
make hands rs e = Views.make $ \player ->
  Declaring
    { hand = Vector.index hands player,
      rounds = map (Round.rotate player) rs,
      eldest = e - player
    }
