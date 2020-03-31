module Jasskell.Game where

import           Data.Finite
import           Data.Vector.Sized              ( Vector
                                                , index
                                                , imapM_
                                                )
import           Jasskell.Action
import           Jasskell.Event
import           Jasskell.GameView
import           Jasskell.Message
import           Jasskell.User
import           Jasskell.Round
import           GHC.TypeLits

data Game n = Game { users :: Vector n User
                   , currentIndex :: Finite n
                   , currentRound :: Round n
                   , rounds :: [RoundFinished n]
                   }

playGame :: KnownNat n => Game n -> IO (Game n)
playGame game = do
    let c = currentIndex game
    imapM_ (\i u -> putMessage u $ UpdateGameView $ toGameView i game)
           (users game)
    event <- UserAction c <$> getAction (index (users game) c)
    playGame $ update event game

update :: KnownNat n => Event n -> Game n -> Game n
update event game = case event of
    UserAction ix action -> case action of
        PlayCard c -> case currentRound game of
            Starting _ -> error "Choose a trump first"
            Playing  r -> game { currentRound = playCard ix c r }
            Finished _ -> error "Round already finished"

toGameView :: KnownNat n => Finite n -> Game n -> GameView
toGameView f g = undefined
