module Jasskell.Round
    ( Round(..)
    , RoundPlaying
    , RoundFinished
    , variant
    , players
    , trick
    , playCard
    )
where

import           Data.Finite
import qualified Data.Set                      as Set
import qualified Data.Vector.Sized             as Vector
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

playCard :: KnownNat n => Finite n -> Card -> RoundPlaying n -> Round n
playCard i c r
    | i /= currentPlayer r = error "It's not your turn"
    | otherwise = case trick r of
        Unresolved t ->
            let t' = addCard c t
            in  case t' of
                    Resolved tr | all (Set.null . cards) $ players r' ->
                        Finished $ RoundFinished $ playerPoints
                            (variant r')
                            (tr : tricks r')
                    _ -> Playing r' { trick = t' }
        Resolved t -> Playing r' { trick   = addCard c $ newTrick $ winner var t
                                 , tricks  = t : tricks r
                                 , variant = nextVariant var
                                 }
  where
    r'  = r { players = removeCard c <$> players r }
    var = variant r

playerPoints :: KnownNat n => Variant -> [TrickResolved n] -> Vector n Int
playerPoints var = Vector.accum (+) (Vector.replicate 0) . zipWith
    (\v t -> (fromIntegral $ winner v t, points v t))
    (iterate nextVariant $ var)
