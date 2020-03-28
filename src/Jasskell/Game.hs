module Jasskell.Game where

import           Data.Finite
import           Data.Vector.Sized
import           Jasskell.Action
import           Jasskell.Event
import           Jasskell.Player
import           Jasskell.Round
import           GHC.TypeLits

data Game n = Game { players :: Vector n Player
                   , currentIndex :: Finite n
                   , currentRound :: Round n
                   , rounds :: [RoundFinished n]
                   }

update :: KnownNat n => Event n -> Game n -> Game n
update event game = case event of
    PlayerAction ix action -> case action of
        PlayCard c -> if ix == currentIndex game
            then case currentRound game of
                Starting   -> error "Choose a trump first"
                Playing  r -> game { currentRound = playCard c r }
                Finished _ -> error "Round already finished"
            else error "It's not your turn"
