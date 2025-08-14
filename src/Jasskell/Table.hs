module Jasskell.Table
  ( Table,
    new,
    join,
    JoinError (..),
    Message (..),
    Event (..),
    UpdateResult (..),
  )
where

import Control.Concurrent.STM (STM)
import Control.Concurrent.STM qualified as STM
import Data.Foldable (forM_, toList)
import Data.Functor (($>))
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Vector4 (Index4, Vector4)
import Data.Vector4 qualified as Vector4
import Jasskell.Card (Card)
import Jasskell.GameState (DeclareError, GameState, ShoveError, UnplayableCardReason)
import Jasskell.GameState qualified as GameState
import Jasskell.Player (Player)
import Jasskell.Player qualified as Player
import Jasskell.Variant (Variant)
import System.Random qualified as Random

newtype Table = Table (STM.TVar TableState)

data TableState = TableState
  { seats :: !(Vector4 Seat),
    gameState :: !GameState
  }

data Seat
  = Empty
  | Taken !Player !(STM.TMVar Event)
  | Disconnected Text

new :: Random.StdGen -> IO Table
new gen =
  Table
    <$> STM.newTVarIO
      TableState
        { seats = Vector4.replicate Empty,
          gameState = GameState.new gen
        }

players :: TableState -> [(Index4, Player, STM.TMVar Event)]
players = mapMaybe go . zip [0 ..] . toList . seats
  where
    go (i, s) = case s of
      Taken p var -> Just (i, p, var)
      _ -> Nothing

data Message
  = DeclareVariant Variant
  | Shove
  | PlayCard Card
  | Disconnect
  deriving (Show)

data UpdateResult
  = Ok
  | NotYourTurn
  | DeclareError !DeclareError
  | ShoveError !ShoveError
  | UnplayableCard !UnplayableCardReason
  deriving (Show)

data Event = Event
  deriving (Show)

data JoinError = TableIsFull
  deriving (Show)

join ::
  Table ->
  Player ->
  STM (Either JoinError (Message -> STM UpdateResult, STM Event))
join t@(Table var) p = do
  ts <- STM.readTVar var
  case Vector4.findIndex canTake (seats ts) of
    Nothing -> pure $ Left TableIsFull
    Just i -> do
      msgVar <- STM.newEmptyTMVar
      writeAndBroadcast
        var
        ts {seats = Vector4.set i (Taken p msgVar) (seats ts)}
      pure $ Right (updateTableState t i, STM.takeTMVar msgVar)
  where
    canTake = \case
      Empty -> True
      Disconnected name -> name == Player.name p
      Taken _ _ -> False

writeAndBroadcast :: STM.TVar TableState -> TableState -> STM ()
writeAndBroadcast !var !ts = do
  STM.writeTVar var ts
  forM_ (players ts) $ \(_, _, eventVar) ->
    STM.writeTMVar eventVar Event

updateTableState :: Table -> Index4 -> Message -> STM UpdateResult
updateTableState (Table var) i msg = do
  ts <- STM.readTVar var
  let move :: (e -> UpdateResult) -> (GameState -> Either e GameState) -> STM UpdateResult
      move toError update
        | GameState.currentPlayer (gameState ts) /= i = pure NotYourTurn
        | otherwise = case update (gameState ts) of
            Left e -> pure $ toError e
            Right gs -> writeAndBroadcast var ts {gameState = gs} $> Ok
  case msg of
    DeclareVariant v ->
      move DeclareError $ GameState.declareVariant v
    Shove ->
      move ShoveError GameState.shove
    PlayCard card ->
      move UnplayableCard $ GameState.playCard card
    Disconnect ->
      let name = case Vector4.index i (seats ts) of
            Empty -> error "Jasskell.Table.uppdateTableState: disconnect from empty seat"
            Taken p _ -> Player.name p
            Disconnected n -> n
       in writeAndBroadcast
            var
            ts {seats = Vector4.set i (Disconnected name) (seats ts)}
            $> Ok
