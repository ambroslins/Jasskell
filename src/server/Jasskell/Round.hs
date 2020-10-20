module Jasskell.Round
  ( Round (..),
    RoundPlaying,
    RoundFinished,
    variant,
    players,
    trick,
    playCard,
    startRound,
    currentPlayer,
    validCards,
  )
where

import Data.Finite
import qualified Data.Set as Set
import Data.Vector.Sized
  ( Vector,
    index,
  )
import qualified Data.Vector.Sized as Vector
import GHC.TypeLits
import Jasskell.Card
import Jasskell.Player
import Jasskell.Trick
import Jasskell.Variant

data RoundPlaying n = RoundPlaying
  { variant :: Variant,
    players :: Vector n Player,
    trick :: Trick n,
    tricks :: [TrickResolved n]
  }

newtype RoundFinished n = RoundFinished (Vector n Int)

data Round n
  = Starting (Vector n Player)
  | Playing (RoundPlaying n)
  | Finished (RoundFinished n)

startRound :: Variant -> Finite n -> Vector n Player -> RoundPlaying n
startRound var ix ps =
  RoundPlaying
    { variant = var,
      players = ps,
      trick = Unresolved $ newTrick ix,
      tricks = []
    }

playCard :: KnownNat n => Card -> RoundPlaying n -> Round n
playCard c r = case trick r of
  Unresolved t ->
    let t' = addCard c t
     in case t' of
          Resolved tr
            | all (Set.null . cards) $ players r' ->
              Finished $
                RoundFinished $
                  playerPoints
                    (variant r')
                    (tr : tricks r')
          _ -> Playing r' {trick = t'}
  Resolved t ->
    Playing
      r'
        { trick = addCard c $ newTrick $ winner var t,
          tricks = t : tricks r,
          variant = nextVariant var
        }
  where
    r' = r {players = removeCard c <$> players r}
    var = variant r

playerPoints :: KnownNat n => Variant -> [TrickResolved n] -> Vector n Int
playerPoints var =
  Vector.accum (+) (Vector.replicate 0)
    . zipWith
      (\v t -> (fromIntegral $ winner v t, points v t))
      (iterate nextVariant var)

currentPlayer :: KnownNat n => RoundPlaying n -> Finite n
currentPlayer r = case trick r of
  Unresolved t -> currentIndex t
  Resolved t -> winner (variant r) t

validCards :: KnownNat n => RoundPlaying n -> Finite n -> Cards
validCards r i =
  if i /= currentPlayer r
    then Set.empty
    else case trick r of
      Resolved _ -> hand
      Unresolved t -> playableCards (variant r) (playedCards t) hand
  where
    hand = cards $ index (players r) i
