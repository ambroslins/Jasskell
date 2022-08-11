module Jasskell.View
  ( View,
    Views,
    makeDeclaring,
    makePlaying,
  )
where

import Data.Finite (Finite)
import Data.Vector.Sized (Vector)
import Data.Vector.Sized qualified as Vector
import Data.Vector.Sized.Extra qualified as Vector
import Jasskell.Card (Card, Cards)
import Jasskell.Variant (Variant)

data View n = View
  { hand :: Cards,
    table :: Vector n (Maybe Card),
    variant :: Maybe Variant,
    leader :: Finite n
  }

type Views n = Finite n -> View n

makeDeclaring :: KnownNat n => Vector n Cards -> Finite n -> Views n
makeDeclaring hs eldest i =
  View
    { hand = Vector.index hs i,
      table = Vector.replicate Nothing,
      variant = Nothing,
      leader = eldest - i
    }

makePlaying ::
  KnownNat n =>
  Vector n Cards ->
  Variant ->
  Vector n (Maybe Card) ->
  Finite n ->
  Views n
makePlaying hs v cs l i =
  View
    { hand = Vector.index hs i,
      table = Vector.rotate i cs,
      variant = Just v,
      leader = l - i
    }
