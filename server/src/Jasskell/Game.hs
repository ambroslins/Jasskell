module Jasskell.Game where

import           Data.Finite
import           Data.Foldable                  ( toList )
import           Data.Vector.Sized              ( Vector
                                                , index
                                                , imapM_
                                                , imap
                                                )
import           Jasskell.Action
import           Jasskell.Event
import           Jasskell.GameView
import           Jasskell.Message
import           Jasskell.Player
import           Jasskell.Round
import           Jasskell.User
import           Jasskell.Trick
import           GHC.TypeLits

data Game n = Game { users :: Vector n User
                   , currentUser :: Finite n
                   , currentRound :: Round n
                   , rounds :: [RoundFinished n]
                   }

playGame :: KnownNat n => Game n -> IO (Game n)
playGame game = do
    let c = currentUser game
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
        ChooseVariant v -> case currentRound game of
            Starting ps -> if ix == currentUser game
                then game { currentRound = Playing $ startRound v ix ps }
                else error "You can't choose the variant"
            _ -> error "You can't choose a trump now"

toGameView :: KnownNat n => Finite n -> Game n -> GameView
toGameView ix g = case currentRound g of
    Starting vec -> GameView
        { hand        = cards $ index vec ix
        , table       = toList $ (\u -> (name u, Nothing)) <$> users g
        , variantView = Nothing
        }
    Playing r -> GameView
        { hand        = cards $ index (players r) ix
        , table       =
            toList $ imap (\i u -> (name u, playedCard i $ trick r)) $ users g
        , variantView = Just $ variant r
        }
    Finished _ -> undefined

