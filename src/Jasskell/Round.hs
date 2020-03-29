module Jasskell.Round where

import           Data.Vector.Sized
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

playCard :: KnownNat n => Card -> RoundPlaying n -> Round n
playCard c r = case trick r of
    Unresolved t -> Playing $ r { trick = addCard c t }
    Resolved   _ -> error "start next trick"

