module Jasskell.Round
    ( Round(..)
    , RoundPlaying
    , RoundFinished
    , playCard
    )
where

import           Data.Finite
import           Data.Vector.Sized              ( Vector )
import           Jasskell.Card
import           Jasskell.Player
import           Jasskell.Trick
import           Jasskell.Variant
import           GHC.TypeLits

data RoundPlaying n = RoundPlaying { variant :: Variant
                                   , players :: Vector n Player
                                   , currentPlayer :: Finite n
                                   , trick :: Trick n
                                   , tricks :: [TrickResolved n]
                                   }

newtype RoundFinished n = RoundFinished (Vector n Int)

data Round n = Starting (Vector n Player)
             | Playing (RoundPlaying n)
             | Finished (RoundFinished n)

playCard :: KnownNat n => Finite n -> Card -> RoundPlaying n -> RoundPlaying n
playCard i c r
    | i /= currentPlayer r = error "It's not your turn"
    | otherwise = case trick r of
        Unresolved t -> r' { trick = addCard c t }
        Resolved   t -> r' { trick   = addCard c $ newTrick $ winner var t
                           , tricks  = t : tricks r
                           , variant = nextVariant var
                           }
  where
    r'  = r { players = removeCard c <$> players r }
    var = variant r

