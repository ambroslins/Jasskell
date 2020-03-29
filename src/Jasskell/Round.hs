module Jasskell.Round where

import           Data.List                      ( foldl' )
import           Data.Vector.Sized              ( Vector )
import           Jasskell.Card
import           Jasskell.Trick
import           Jasskell.Variant
import           GHC.TypeLits

data RoundPlaying n = RoundPlaying { variant :: Variant
                                   , trick :: Trick n
                                   , tricks :: [TrickResolved n]
                                   }

data RoundFinished n = RoundFinished (Vector n Int)

data Round n = Starting
           | Playing (RoundPlaying n)
           | Finished (RoundFinished n)

currentVariant :: RoundPlaying n -> Variant
currentVariant r = foldl' (const . nextVariant) (variant r) $ tricks r

playCard :: KnownNat n => Card -> RoundPlaying n -> RoundPlaying n
playCard c r = case trick r of
    Unresolved t -> r { trick = addCard c t }
    Resolved   t -> r
        { tricks = t : tricks r
        , trick  = addCard c $ newTrick $ winner (currentVariant r) t
        }

