module View
  ( View,
    Declaring (..),
    Playing (..),
    declaring,
    playing,
  )
where

import Card (Card, Cards)
import Data.Finite (Finite)
import Data.Vector.Sized (Vector)
import Data.Vector.Sized qualified as Vector
import GHC.TypeNats (KnownNat)
import List qualified
import Map qualified
import Variant (Variant)

type View v n = Finite n -> v n

data Declaring n = Declaring
  { hand :: Cards,
    eldest :: Finite n
  }
  deriving (Show)

data Playing n = Playing
  { hand :: Cards,
    leader :: Finite n,
    variant :: Variant,
    trick :: Vector n (Maybe Card)
  }
  deriving (Show)

declaring :: KnownNat n => Finite n -> Vector n Cards -> View Declaring n
declaring eldest hands player =
  Declaring
    { eldest = eldest - player,
      hand = Vector.index hands player
    }

playing :: KnownNat n => Finite n -> Variant -> Vector n Cards -> [Card] -> View Playing n
playing leader variant hands cards player =
  Playing
    { hand = Vector.index hands player,
      leader = leader - player,
      variant = variant,
      trick = Vector.generate (`Map.lookup` cardMap)
    }
  where
    cardMap = Map.fromList $ List.zip [(leader - player) ..] cards
