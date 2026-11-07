{-# LANGUAGE QuasiQuotes #-}

module Jasskell.Game
  ( Manager,
    withManager,
    GameId,
    PublicGame (..),
    create,
    getPublicGames,
    join,
    Connection (..),
    Message (..),
    Action (..),
  )
where

import Control.Concurrent (forkIOWithUnmask)
import Control.Concurrent.MVar (MVar, modifyMVarMasked, modifyMVar_, newMVar, readMVar)
import Control.Concurrent.STM (STM)
import Control.Concurrent.STM qualified as STM
import Control.Exception (SomeException (..), bracket, evaluate, try)
import Control.Monad (forM_, unless)
import Data.Aeson (FromJSON, ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.Bifunctor (bimap)
import Data.Data (Proxy (..))
import Data.Fin (Fin)
import Data.Fin qualified as Fin
import Data.IntMap.Coercible (IntMap)
import Data.IntMap.Coercible qualified as IntMap
import Data.Maybe (isJust, isNothing)
import Data.Profunctor (Profunctor (lmap), dimap)
import Data.Time (UTCTime, getCurrentTime)
import Data.Type.Equality (type (:~:) (Refl))
import Data.Vec (Vec)
import Data.Vec qualified as Vec
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import GHC.Generics (Generic)
import GHC.TypeLits (KnownNat, sameNat)
import Hasql.Session qualified as Hasql
import Hasql.Statement qualified as Hasql
import Hasql.TH (maybeStatement, resultlessStatement, rowsAffectedStatement, vectorStatement)
import Jasskell.Card (Card)
import Jasskell.Database qualified as DB
import Jasskell.Dealer qualified as Dealer
import Jasskell.Id (Id (..))
import Jasskell.Id qualified as Id
import Jasskell.Logger
import Jasskell.Player (Nickname (..), Player (..), PlayerId)
import Jasskell.Player qualified as Player
import Jasskell.Round (Round)
import Jasskell.Round qualified as Round
import Jasskell.Throw
import Jasskell.Variant (Variant)
import Jasskell.View qualified as View
import System.Random.Stateful qualified as Random
import Prelude hiding (round)

type GameId = Id Game

data Some f = forall n. (KnownNat n) => Some (f n)

newtype Manager = Manager
  { games :: MVar (IntMap GameId (Some Game))
  }

data Game n = Game
  { id :: GameId,
    ioGen :: Random.IOGenM Random.StdGen,
    eventQueue :: STM.TBQueue (Event n),
    clients :: STM.TVar (IntMap PlayerId (Client n)),
    closed :: STM.TVar Bool
  }

withManager :: (Manager -> IO a) -> IO a
withManager f = do
  tm <- Manager <$> newMVar IntMap.empty
  -- TODO: reaper thread
  -- TODO: stop all games on cleanup
  f tm

data Client n = Client
  { player :: Player,
    state :: ClientState n
  }
  deriving (Eq)

data ClientState n
  = Connected (STM.TMVar (Message n))
  | Disconnected UTCTime
  deriving (Eq)

data Connection n = Connection
  { receive :: STM (Message n),
    send :: Action n -> STM ()
  }

data Event n
  = PlayerJoined PlayerId
  | PlayerLeft PlayerId
  | PlayerAction PlayerId (Action n)
  deriving (Eq, Show)

data Action n
  = TakeSeat (Fin n)
  | StartGame
  | DeclareVariant Variant
  | PlayCard Card
  deriving (Eq, Show, Generic)

data Message n
  = UpdateWaiting (View.Waiting n)
  | UpdatePlayer (View.Playing n)
  | UpdateSpectator (View.Spectating n)
  | Rejected (Rejection n)
  | Superseded
  | GameClosed
  deriving (Show)

data Rejection n
  = SeatTaken
  | AlreadySeated
  | NotAPlayer
  | VariantAlreadyDeclared
  | GameAlreadyStarted
  | GameNotStarted
  | NotYourTurn
  | PlayError Round.PlayError
  deriving (Show)

data GameState n
  = Waiting (Vec n (Maybe Player))
  | Playing (Vec n Player) (Round n)
  deriving (Show)

create :: DB.Pool -> PlayerId -> IO GameId
create db createdBy = do
  gameId <- Id.new
  DB.use db $ Hasql.statement (gameId, createdBy) insertGame
  pure gameId

insertGame :: Hasql.Statement (GameId, PlayerId) ()
insertGame =
  lmap
    (bimap Id.toInt64 Id.toInt64)
    [resultlessStatement|
      insert into games (game_id, created_by)
      values ($1::int8, $2::int8)
    |]

data PublicGame = PublicGame
  { id :: GameId,
    creatorName :: Nickname,
    seatsTaken :: Int
  }
  deriving (Show)

getPublicGames :: DB.Pool -> IO (Vector PublicGame)
getPublicGames db = DB.use db $ Hasql.statement () selectPublicGames

-- TODO: ignore active games
selectPublicGames :: Hasql.Statement () (Vector PublicGame)
selectPublicGames =
  fmap
    ( Vector.map $ \(gameId, nickname, seatsTaken) ->
        PublicGame
          { id = Id.fromInt64 gameId,
            creatorName = Nickname nickname,
            seatsTaken = fromIntegral seatsTaken
          }
    )
    [vectorStatement|
      select
        g.game_id::int8,
        any_value(p.nickname)::text,
        count(s.game_id)::int8 as seats_taken
      from games as g
      join players as p on p.player_id = g.created_by
      left join seats as s on s.game_id = g.game_id
      group by g.game_id
      order by seats_taken desc, g.game_id asc
    |]

join ::
  Logger ->
  DB.Pool ->
  Player ->
  GameId ->
  Manager ->
  (forall n. (KnownNat n) => Connection n -> IO a) ->
  IO (Maybe a)
join logger db player gameId manager handler = handling pure $ \notFound -> do
  games <- readMVar manager.games
  Some game <- case IntMap.lookup gameId games of
    Nothing ->
      getGameSnapshot db gameId >>= \case
        Nothing -> throw notFound Nothing
        Just (Some snapshot) -> Some <$> spawnGame logger db snapshot manager
    Just t -> pure t
  messageBox <- STM.newEmptyTMVarIO
  let client = Client {player, state = Connected messageBox}
      connection =
        Connection
          { receive = do
              closed <- STM.readTVar game.closed
              if closed
                then pure GameClosed
                else STM.takeTMVar messageBox,
            send = STM.writeTBQueue game.eventQueue . PlayerAction player.id
          }
      acquire = STM.atomically $ do
        clients <- STM.readTVar game.clients
        case IntMap.lookup player.id clients of
          Just Client {state = Connected box} -> STM.writeTMVar box Superseded
          _ -> pure ()
        STM.writeTVar game.clients $!
          IntMap.insert player.id client clients
        STM.writeTBQueue game.eventQueue $ PlayerJoined player.id
      release _ = do
        disconnected <- Disconnected <$> getCurrentTime
        STM.atomically $ do
          STM.modifyTVar' game.clients $
            IntMap.adjust
              (\c -> if c == client then client {state = disconnected} else c)
              player.id
          STM.writeTBQueue game.eventQueue $ PlayerLeft player.id
  bracket acquire release $ \() -> Just <$> handler connection

spawnGame :: forall n. (KnownNat n) => Logger -> DB.Pool -> Snapshot n -> Manager -> IO (Game n)
spawnGame l db snapshot manager = do
  eventQueue <- STM.newTBQueueIO 16
  clients <- STM.newTVarIO @(IntMap PlayerId (Client n)) IntMap.empty
  let !state = restoreState snapshot
  modifyMVarMasked manager.games $ \games ->
    case IntMap.lookup snapshot.id games of
      Just (Some game) -> case sameNat snapshot game of
        Nothing -> error "bad nat"
        Just Refl -> pure (games, game)
      Nothing -> do
        closed <- STM.newTVarIO False
        ioGen <- Random.newStdGen >>= Random.newIOGenM
        let !game =
              Game
                { id = snapshot.id,
                  ioGen,
                  eventQueue,
                  clients,
                  closed
                } ::
                Game n
        _threadId <- forkIOWithUnmask $ \unmask -> do
          result <- try $ unmask $ do
            logger.debug "game started" []
            gameLoop logger db game state
          modifyMVar_ manager.games (evaluate . IntMap.delete snapshot.id)
          STM.atomically $ STM.writeTVar closed True
          unmask $ case result of
            Left (SomeException e) -> logger.error "game crashed" ["exception" =: show e]
            Right () -> logger.debug "game closed" []

        let !ts = IntMap.insert snapshot.id (Some game) games
        pure (ts, game)
  where
    logger = withContext l ["game_id" =: Id.encodeText snapshot.id]

restoreState :: Snapshot n -> GameState n
restoreState snapshot = Waiting snapshot.seats

data Snapshot n = Snapshot
  { id :: GameId,
    creator :: PlayerId,
    seats :: Vec n (Maybe Player)
  }

getGameSnapshot :: DB.Pool -> GameId -> IO (Maybe (Some Snapshot))
getGameSnapshot db gameId =
  DB.use db (Hasql.statement gameId selectGameSnapshot)

selectGameSnapshot :: Hasql.Statement GameId (Maybe (Some Snapshot))
selectGameSnapshot =
  dimap
    Id.toInt64
    (fmap decode)
    [maybeStatement|
      select
        g.game_id::int8,
        g.created_by::int8,
        coalesce(s.seats, '{}'::int4[])::int4[],
        coalesce(s.player_ids, '{}'::int8[])::int8[],
        coalesce(s.nicknames, '{}'::text[])::text[]
      from games as g
      left join lateral (
        select
          array_agg(s.seat order by s.seat) as seats,
          array_agg(s.player_id order by s.seat) as player_ids,
          array_agg(p.nickname order by s.seat) as nicknames
        from seats as s
        left join players as p on p.player_id = s.player_id
        where s.game_id = g.game_id and p.player_id is not null
      ) as s on true
      where game_id = $1::int8 and created_by is not null
    |]
  where
    decode (gameId, creatdBy, indices, playerIds, nicknames) =
      Some $
        Snapshot
          { id = Id.fromInt64 gameId,
            creator = Id.fromInt64 creatdBy,
            seats =
              Vector.foldl' setSeat (Vec.replicate Nothing) $
                Vector.zip3 indices playerIds nicknames
          }
    setSeat v (i, p, n) =
      Vec.set @4
        (fromIntegral i)
        (Just $ Player {id = Id.fromInt64 p, nickname = Player.Nickname n})
        v

gameLoop :: (KnownNat n) => Logger -> DB.Pool -> Game n -> GameState n -> IO ()
gameLoop logger db game = go
  where
    go state = do
      event <- STM.atomically $ STM.readTBQueue game.eventQueue
      logger.debug "got game event" ["event" =: show event]
      clients <- STM.readTVarIO game.clients
      case event of
        PlayerJoined playerId -> do
          case IntMap.lookup playerId clients of
            Just Client {state = Connected box} ->
              STM.atomically $ STM.writeTMVar box $ renderState state playerId
            _ -> pure ()
          go state
        PlayerLeft _ -> go state
        PlayerAction playerId action ->
          go =<< case IntMap.lookup playerId clients of
            Nothing -> pure state
            Just Client {player, state = clientState} ->
              applyAction db game state player action >>= \case
                Left rejection -> case clientState of
                  Disconnected _ -> pure state
                  Connected box -> do
                    STM.atomically $ STM.writeTMVar box $ Rejected rejection
                    pure state
                Right s -> do
                  let render = renderState s
                  forM_ clients $ \client -> case client.state of
                    Disconnected _ -> pure ()
                    Connected box -> STM.atomically $ STM.writeTMVar box $ render client.player.id
                  pure s
    renderState state = case state of
      Waiting seats -> UpdateWaiting . View.makeWaiting seats
      Playing players round -> \playerId ->
        case Vec.findIndex (\p -> p.id == playerId) players of
          Nothing -> UpdateSpectator $ View.makeSpectating players
          Just i -> UpdatePlayer $ View.makePlaying players round i

applyAction :: (KnownNat n) => DB.Pool -> Game n -> GameState n -> Player -> Action n -> IO (Either (Rejection n) (GameState n))
applyAction db game state player action = trying $ \reject -> case action of
  TakeSeat seat -> case state of
    Playing _ _ -> throw reject GameAlreadyStarted
    Waiting seats
      | isJust playerSeat -> throw reject AlreadySeated
      | isJust (Vec.index seats seat) -> throw reject SeatTaken
      | otherwise -> do
          ok <- DB.use db $ Hasql.statement (game.id, player.id, seat) insertSeat
          unless ok $ error "seat insert failed"
          pure $ Waiting $ Vec.set seat (Just player) seats
  StartGame -> case state of
    Playing _ _ -> throw reject GameAlreadyStarted
    Waiting seats -> case Vec.sequence seats of
      Nothing -> error "game not full"
      Just s
        | isNothing playerSeat -> throw reject NotAPlayer
        | otherwise -> do
            hands <- case sameNat game (Proxy @4) of
              Nothing -> error "only implemented for 4 players"
              Just Refl -> Random.applyIOGen Dealer.four game.ioGen
            let chooser = 0 -- TODO: choose based on weli
                round = Round.start chooser hands
            -- TODO: Save game to database
            pure $ Playing s round
  DeclareVariant variant -> case state of
    Waiting _ -> throw reject GameNotStarted
    Playing seats round -> do
      checkCurrentPlayer reject round
      case Round.choose variant round of
        Left _ -> throw reject VariantAlreadyDeclared
        Right gs -> pure $ Playing seats gs
  PlayCard card -> case state of
    Waiting _ -> throw reject GameNotStarted
    Playing seats round -> do
      checkCurrentPlayer reject round
      case Round.playCard card round of
        Left playError -> throw reject $ PlayError playError
        Right gs -> do
          -- TODO: save played card event
          pure $ Playing seats gs
  where
    playerSeat = case state of
      Waiting seats -> Vec.findIndex (maybe False $ \p -> p.id == player.id) seats
      Playing seats _ -> Vec.findIndex (\p -> p.id == player.id) seats
    checkCurrentPlayer reject gs = case playerSeat of
      Nothing -> throw reject NotYourTurn
      Just s
        | s /= Round.currentPlayer gs -> throw reject NotYourTurn
        | otherwise -> pure ()

insertSeat :: Hasql.Statement (GameId, PlayerId, Fin n) Bool
insertSeat =
  dimap
    encode
    (> 0)
    [rowsAffectedStatement|
      insert into seats (game_id, player_id, seat)
      values ($1::int8, $2::int8, $3::int4)
      on conflict (game_id, seat) do update
        set player_id = excluded.player_id
      where seats.player_id is null
    |]
  where
    encode (gameId, playerId, seat) =
      (Id.toInt64 gameId, Id.toInt64 playerId, fromIntegral $ Fin.toInt seat)

instance (KnownNat n) => ToJSON (Action n) where
  toJSON = Aeson.genericToJSON Aeson.defaultOptions

instance (KnownNat n) => FromJSON (Action n) where
  parseJSON = Aeson.genericParseJSON Aeson.defaultOptions
