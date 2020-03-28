module Jasskell.Game where

import           Data.Finite
import           Data.Vector.Sized              ( Vector
                                                , index
                                                )
import           Control.Monad                  ( forM_ )
import           Jasskell.Action
import           Jasskell.Event
import           Jasskell.GameView
import           Jasskell.Message
import           Jasskell.Player
import           Jasskell.Round
import           GHC.TypeLits

data Game n = Game { players :: Vector n Player
                   , currentIndex :: Finite n
                   , currentRound :: Round n
                   , rounds :: [RoundFinished n]
                   }

playGame :: KnownNat n => Game n -> IO (Game n)
playGame game = do
    let c = currentIndex game
    forM_ (players game) (\p -> putMessage p $ UpdateGameView $ toGameView game)
    event <- PlayerAction c <$> (getAction $ index (players game) c)
    playGame $ update event game

update :: KnownNat n => Event n -> Game n -> Game n
update event game = case event of
    PlayerAction ix action -> case action of
        PlayCard c -> if ix == currentIndex game
            then case currentRound game of
                Starting   -> error "Choose a trump first"
                Playing  r -> game { currentRound = playCard c r }
                Finished _ -> error "Round already finished"
            else error "It's not your turn"

toGameView :: Game n -> GameView
toGameView = undefined
