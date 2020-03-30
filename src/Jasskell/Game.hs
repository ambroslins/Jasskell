module Jasskell.Game where

import           Data.Finite
import qualified Data.Vector.Sized             as Vector
import           Data.Vector.Sized              ( Vector
                                                , index
                                                , imapM_
                                                , toList
                                                , imap
                                                )
import           Jasskell.Action
import           Jasskell.Event
import           Jasskell.GameView
import           Jasskell.Message
import           Jasskell.Player
import           Jasskell.Round
import           Jasskell.Trick
import           GHC.TypeLits

data Game n = Game { players :: Vector n Player
                   , currentIndex :: Finite n
                   , currentRound :: Round n
                   , rounds :: [RoundFinished n]
                   }

playGame :: KnownNat n => Game n -> IO (Game n)
playGame game = do
    let c = currentIndex game
    imapM_ (\i p -> putMessage p $ UpdateGameView $ toGameView i game)
           (players game)
    event <- PlayerAction c <$> getAction (index (players game) c)
    playGame $ update event game

update :: KnownNat n => Event n -> Game n -> Game n
update event game = case event of
    PlayerAction ix action -> case action of
        PlayCard c -> if ix == currentIndex game
            then case currentRound game of
                Starting  -> error "Choose a trump first"
                Playing r -> game
                    { players      = Vector.map (removeCard c) $ players game
                    , currentRound = Playing $ playCard c r
                    }
                Finished _ -> error "Round already finished"
            else error "It's not your turn"

toGameView :: KnownNat n => Finite n -> Game n -> GameView
toGameView f g = GameView
    { hand        = cards $ index (players g) $ currentIndex g
    , table       =
        toList
        $ imap (\i p -> (name p, card i))
        $ rotateN (toInteger f)
        $ players g
    , variantView = case currentRound g of
                        Playing r -> Just $ variant r
                        _         -> Nothing
    }
  where
    card x = case currentRound g of
        Playing r -> playedCard x $ trick r
        _         -> Nothing
