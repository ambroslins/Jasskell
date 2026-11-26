{-# LANGUAGE QuasiQuotes #-}

module Jasskell.Game
  ( Manager,
    withManager,
    GameId,
    PublicGame (..),
    create,
    getPublicGames,
    GameInfo (..),
    getGameInfo,
    join,
    Message (..),
    Rejection (..),
    Action (..),
  )
where

import Control.Applicative ((<|>))
import Control.Concurrent (forkIOWithUnmask)
import Control.Concurrent.MVar (MVar, modifyMVarMasked, modifyMVar_, newMVar, readMVar)
import Control.Concurrent.STM (STM)
import Control.Concurrent.STM qualified as STM
import Control.Exception (SomeException (..), bracket, evaluate, try)
import Control.Monad (foldM, forM_, unless, when)
import Data.Aeson (FromJSON (..), ToJSON (..), (.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson.Types
import Data.Bifunctor (first)
import Data.BitSet (BitSet)
import Data.BitSet qualified as BitSet
import Data.Bits qualified as Bits
import Data.Data (Proxy (..))
import Data.Fin (Fin)
import Data.Fin qualified as Fin
import Data.Functor (($>))
import Data.Int (Int16, Int32, Int64)
import Data.IntMap.Coercible (IntMap)
import Data.IntMap.Coercible qualified as IntMap
import Data.Maybe (isJust, isNothing)
import Data.Profunctor (dimap, lmap)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Type.Equality (type (:~:) (Refl))
import Data.Vec (Vec)
import Data.Vec qualified as Vec
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU
import GHC.Generics (Generic)
import GHC.TypeLits (KnownNat, sameNat)
import Hasql.Session qualified as Hasql
import Hasql.Statement qualified as Hasql
import Hasql.TH
  ( maybeStatement,
    resultlessStatement,
    rowsAffectedStatement,
    vectorStatement,
  )
import Hasql.Transaction qualified as Transaction
import Jasskell.Card (Card)
import Jasskell.Card qualified as Card
import Jasskell.Card.Set (CardSet)
import Jasskell.Card.Set qualified as CardSet
import Jasskell.Clock (Clock, Duration, Timestamp, seconds)
import Jasskell.Clock qualified as Clock
import Jasskell.Database qualified as DB
import Jasskell.Dealer qualified as Dealer
import Jasskell.Id (Id (..))
import Jasskell.Id qualified as Id
import Jasskell.Logger
import Jasskell.Player (Nickname (..), Player (..), PlayerId)
import Jasskell.Round (Round)
import Jasskell.Round qualified as Round
import Jasskell.Throw
import Jasskell.Variant (Variant)
import Jasskell.Variant qualified as Variant
import Jasskell.View qualified as View
import System.Random.Stateful qualified as Random
import Prelude hiding (round)

type GameId = Id Game

data Some f = forall n. (KnownNat n) => Some (f n)

data Manager = Manager
  { games :: MVar (IntMap GameId (Some Game)),
    clock :: Clock
  }

data Game n = Game
  { id :: GameId,
    ioGen :: Random.IOGenM Random.StdGen,
    clock :: Clock,
    eventQueue :: STM.TBQueue (Event n),
    clients :: STM.TVar (IntMap PlayerId (Client n)),
    closed :: STM.TVar Bool
  }

withManager :: Clock -> (Manager -> IO a) -> IO a
withManager clock f = do
  games <- newMVar IntMap.empty
  let tm = Manager {games, clock}
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
  | Disconnected Timestamp
  deriving (Eq)

data Event n
  = PlayerJoined PlayerId
  | PlayerLeft PlayerId
  | PlayerAction PlayerId (Action n)
  | Tick
  deriving (Eq, Show)

data Action n
  = TakeSeat (Fin n)
  | StartGame
  | -- | Schieben: the chooser passes the choice of variant to their partner.
    Shove
  | DeclareVariant Variant
  | -- | The flag skips the player's Weis; only allowed with their first card.
    PlayCard Card Bool
  | -- | Deal the next round once the current one is finished.
    NextRound
  deriving (Eq, Show, Generic)

data Message n
  = UpdateWaiting (View.Waiting n)
  | UpdatePlayer (View.Playing n)
  | UpdateSpectator (View.Spectating n)
  | Rejected (Rejection n)
  | Tickle Timestamp
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
  | ShoveError Round.ShoveError
  | PlayError Round.PlayError
  | RoundNotFinished
  | MeldAlreadyAnnounced
  deriving (Show)

newtype RoundNumber = RoundNumber {toInt16 :: Int16}
  deriving (Eq, Show)

data Phase n
  = Waiting (Vec n (Maybe Player))
  | Playing (GameState n)
  deriving (Show)

data GameState n = GameState
  { players :: Vec n Player,
    -- | Total points per team of the finished rounds before the current one.
    -- The current round's result is computed from 'round'.
    score :: Vec 2 Int,
    roundNumber :: RoundNumber,
    cardSeqNumber :: Int16,
    round :: Round n
  }
  deriving (Show)

-- | Create a new game. Private games are not listed on the landing page and
-- can only be joined through their link.
create :: DB.Pool -> PlayerId -> Bool -> IO GameId
create db createdBy private = do
  gameId <- Id.new
  DB.use db $ Hasql.statement (gameId, createdBy, private) insertGame
  pure gameId

insertGame :: Hasql.Statement (GameId, PlayerId, Bool) ()
insertGame =
  lmap
    (\(gameId, playerId, private) -> (Id.toInt64 gameId, Id.toInt64 playerId, private))
    [resultlessStatement|
      insert into games (game_id, created_by, private)
      values ($1::int8, $2::int8, $3::bool)
    |]

data PublicGame = PublicGame
  { id :: GameId,
    creatorName :: Nickname,
    seatsTaken :: Int
  }
  deriving (Show)

getPublicGames :: DB.Pool -> IO (Vector PublicGame)
getPublicGames db = DB.use db $ Hasql.statement () selectPublicGames

selectPublicGames :: Hasql.Statement () (Vector PublicGame)
selectPublicGames =
  fmap
    ( V.map $ \(gameId, nickname, seatsTaken) ->
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
      where not g.private
        and not exists (select 1 from rounds as r where r.game_id = g.game_id)
      group by g.game_id
      order by seats_taken desc, g.game_id asc
    |]

-- | Summary of a single game (public or private), shown before joining it.
data GameInfo = GameInfo
  { creatorName :: Maybe Nickname,
    seatsTaken :: Int,
    started :: Bool
  }
  deriving (Show)

getGameInfo :: DB.Pool -> GameId -> IO (Maybe GameInfo)
getGameInfo db gameId = DB.use db $ Hasql.statement gameId selectGameInfo

selectGameInfo :: Hasql.Statement GameId (Maybe GameInfo)
selectGameInfo =
  dimap
    Id.toInt64
    ( fmap $ \(nickname, seatsTaken, started) ->
        GameInfo
          { creatorName = Nickname <$> nickname,
            seatsTaken = fromIntegral seatsTaken,
            started
          }
    )
    [maybeStatement|
      select
        p.nickname::text?,
        (
          select count(*) from seats as s
          where s.game_id = g.game_id and s.player_id is not null
        )::int8,
        exists (select 1 from rounds as r where r.game_id = g.game_id)::bool
      from games as g
      left join players as p on p.player_id = g.created_by
      where g.game_id = $1::int8
    |]

join ::
  Logger ->
  DB.Pool ->
  Player ->
  GameId ->
  Manager ->
  (forall n. (KnownNat n) => (Action n -> STM ()) -> STM (Message n) -> IO a) ->
  IO (Maybe a)
join logger db player gameId manager handler = handling pure $ \notFound -> do
  games <- readMVar manager.games
  Some game <- case IntMap.lookup gameId games of
    Nothing ->
      DB.statement db selectStoredGame gameId >>= \case
        Nothing -> throw notFound Nothing
        Just (Some sg) -> case restore sg of
          Left err -> do
            logger.error "failed to restore stored game" ["error" =: err]
            throw notFound Nothing
          Right phase -> Some <$> spawnGame logger db gameId phase manager
    Just t -> pure t
  messageBox <- STM.newEmptyTMVarIO
  let client = Client {player, state = Connected messageBox}
      send = STM.writeTBQueue game.eventQueue . PlayerAction player.id
      receive = do
        closed <- STM.readTVar game.closed
        if closed
          then pure GameClosed
          else STM.takeTMVar messageBox
      acquire = STM.atomically $ do
        clients <- STM.readTVar game.clients
        case IntMap.lookup player.id clients of
          Just Client {state = Connected box} -> STM.writeTMVar box Superseded
          _ -> pure ()
        STM.writeTVar game.clients $!
          IntMap.insert player.id client clients
        STM.writeTBQueue game.eventQueue $ PlayerJoined player.id
      release _ = do
        disconnected <- Disconnected <$> Clock.now game.clock
        STM.atomically $ do
          STM.modifyTVar' game.clients $
            IntMap.adjust
              (\c -> if c == client then client {state = disconnected} else c)
              player.id
          STM.writeTBQueue game.eventQueue $ PlayerLeft player.id
  bracket acquire release $ \() -> Just <$> handler send receive

spawnGame :: forall n. (KnownNat n) => Logger -> DB.Pool -> GameId -> Phase n -> Manager -> IO (Game n)
spawnGame l db gameId phase manager = do
  eventQueue <- STM.newTBQueueIO 16
  clients <- STM.newTVarIO @(IntMap PlayerId (Client n)) IntMap.empty
  modifyMVarMasked manager.games $ \games ->
    case IntMap.lookup gameId games of
      Just (Some game) -> case sameNat phase game of
        Nothing -> error "bad nat"
        Just Refl -> pure (games, game)
      Nothing -> do
        closed <- STM.newTVarIO False
        ioGen <- Random.newStdGen >>= Random.newIOGenM
        let !game = Game {id = gameId, ioGen, clock = manager.clock, eventQueue, clients, closed}
        _threadId <- forkIOWithUnmask $ \unmask -> do
          result <- try $ unmask $ do
            logger.debug "game started" []
            gameLoop logger db game phase
          modifyMVar_ manager.games (evaluate . IntMap.delete gameId)
          STM.atomically $ STM.writeTVar closed True
          unmask $ case result of
            Left (SomeException e) -> logger.error "game crashed" ["exception" =: show e]
            Right () -> logger.debug "game closed" []

        let !ts = IntMap.insert gameId (Some game) games
        pure (ts, game)
  where
    logger = withContext l ["game_id" =: Id.encodeText gameId]

data StoredGame n = StoredGame
  { gameId :: GameId,
    seats :: Vec n (Maybe Player),
    -- | Like 'GameState.score'.
    score :: Vec 2 Int,
    round :: Maybe (StoredRound n)
  }
  deriving (Show)

data StoredRound n = StoredRound
  { roundNumber :: RoundNumber,
    chooser :: Fin n,
    shoved :: Bool,
    variant :: Maybe Variant,
    -- | Seats that skipped their Weis; 'replay' sets their melds to @[]@.
    skippedMelds :: BitSet n,
    hands :: Vec n CardSet,
    cards :: VU.Vector Card
  }
  deriving (Show)

restore :: (KnownNat n) => StoredGame n -> Either Text (Phase n)
restore sg = case sg.round of
  Nothing -> pure $! Waiting sg.seats
  Just sr -> do
    players <- Vec.mapM (note "player is missing") sg.seats
    round <- replay sr
    pure $
      Playing
        GameState
          { players,
            score = sg.score,
            roundNumber = sr.roundNumber,
            cardSeqNumber = fromIntegral $ VU.length sr.cards,
            round
          }

replay :: (KnownNat n) => StoredRound n -> Either Text (Round n)
replay sr = do
  let r0 = Round.start sr.chooser sr.hands
  r1 <- if sr.shoved then first Text.show $ Round.shove r0 else pure r0
  r2 <- maybe (pure r1) (\v -> first Text.show $ Round.choose v r1) sr.variant
  -- Skipping right after the declaration gives the same Weis as skipping with
  -- the first card: the best Weis is only decided when the first trick closes.
  r3 <- foldM skip r2 Fin.fins
  VU.foldM' (\r c -> first Text.show $ Round.playCard c r) r3 sr.cards
  where
    skip r seat
      | BitSet.index sr.skippedMelds seat = first Text.show $ Round.skipMeld seat r
      | otherwise = pure r

-- | Seats, the latest round with its played cards, and the total points per
-- team of the finished rounds before it (@sc@).
selectStoredGame :: Hasql.Statement GameId (Maybe (Some StoredGame))
selectStoredGame =
  lmap Id.toInt64 $
    Hasql.refineResult
      (maybe (pure Nothing) (fmap (Just . Some) . decode))
      [maybeStatement|
      select
        g.game_id::int8,

        s.player_ids::int8?[],
        s.nicknames::text?[],

        r.round_number::int2?,
        r.chooser::int2?,
        r.shoved::bool?,
        r.skipped_melds::int2?,
        r.variant::int2?,
        r.initial_hands::int8[]?,

        pc.cards::int2[],

        sc.team0::int8,
        sc.team1::int8
      from games g

      cross join lateral (
        select
          array_agg(p.player_id order by i) as player_ids,
          array_agg(p.nickname  order by i) as nicknames
        from generate_series(0, 3) as i
        left join seats s on s.game_id = g.game_id and s.seat = i
        left join players p on p.player_id = s.player_id
      ) s

      left join lateral (
        select *
        from rounds r
        where r.game_id = g.game_id
        order by r.round_number desc
        limit 1
      ) r on true

      left join lateral (
        select
          coalesce(array_agg(pc.card      order by pc.seq), '{}') as cards
        from played_cards pc
        where pc.game_id = r.game_id
          and pc.round_number = r.round_number
      ) pc on true

      left join lateral (
        select
          coalesce(sum(f.points[1]), 0) as team0,
          coalesce(sum(f.points[2]), 0) as team1
        from rounds f
        where f.game_id = g.game_id
          and f.round_number < r.round_number
      ) sc on true

      where g.game_id = $1::int8
    |]
  where
    decodeWith msg f x = case f x of
      Nothing -> Left $ msg <> Text.show x
      Just y -> pure y
    makePlayer p n = Player {id = Id.fromInt64 p, nickname = Nickname n}
    decode ::
      (Int64, V.Vector (Maybe Int64), V.Vector (Maybe Text), Maybe Int16, Maybe Int16, Maybe Bool, Maybe Int16, Maybe Int16, Maybe (VU.Vector Int64), VU.Vector Int16, Int64, Int64) ->
      Either Text (StoredGame 4)
    decode (gameId, playerIds, nicknames, mrn, mc, ms, msk, mv, mhs, cs, team0, team1) = do
      let score = Vec.generate $ \team ->
            fromIntegral $ if team == 0 then team0 else team1
      seats <-
        note "invalid seat count" . Vec.fromVector $
          V.zipWith (liftA2 makePlayer) playerIds nicknames
      round <- case (mrn, mc, ms, msk, mhs) of
        (Nothing, Nothing, Nothing, Nothing, Nothing) -> pure Nothing
        (Just rn, Just c, Just shoved, Just sk, Just hs) -> do
          chooser <- decodeWith "invalid chooser: " (Fin.make . fromIntegral) c
          skippedMelds <- decodeWith "invalid skipped melds: " (BitSet.fromWord64 . fromIntegral) sk
          variant <- traverse (decodeWith "invalid variant: " Variant.fromInt16) mv
          hands <- Vec.map CardSet.fromInt64 <$> decodeWith "invalid hands: " Vec.fromVector hs
          cards <- VU.mapM (decodeWith "invalid card: " Card.fromInt16) cs
          pure $ Just StoredRound {roundNumber = RoundNumber rn, chooser, shoved, variant, skippedMelds, hands, cards}
        _ -> Left "incomplete round row"
      pure StoredGame {gameId = Id.fromInt64 gameId, seats, score, round}

tickInterval :: Duration
tickInterval = seconds 5

gameLoop :: (KnownNat n) => Logger -> DB.Pool -> Game n -> Phase n -> IO ()
gameLoop logger db game initial = Clock.delayed game.clock tickInterval Tick >>= go initial
  where
    go !phase tick = do
      event <- STM.atomically $ tick <|> STM.readTBQueue game.eventQueue
      logger.debug "got game event" ["event" =: show event]
      clients <- STM.readTVarIO game.clients
      case event of
        PlayerJoined playerId -> do
          forM_ (IntMap.lookup playerId clients) $
            \client -> clientSend client $ render phase client.player.id
          go phase tick
        PlayerLeft _ -> go phase tick
        PlayerAction playerId action -> do
          nextPhase <- case IntMap.lookup playerId clients of
            Nothing -> pure phase
            Just client ->
              applyAction db game phase client.player action >>= \case
                Left rejection -> clientSend client (Rejected rejection) $> phase
                Right p -> broadcast clients (render p) $> p
          go nextPhase tick
        Tick -> do
          ts <- Clock.now game.clock
          broadcast clients $ const $ Tickle ts
          next <- Clock.delayed game.clock tickInterval Tick
          go phase next
    render = \case
      Waiting seats -> UpdateWaiting . View.makeWaiting seats
      Playing state -> \playerId ->
        case Vec.findIndex (\p -> p.id == playerId) state.players of
          Nothing -> UpdateSpectator $ View.makeSpectating state.players
          Just i -> UpdatePlayer $ View.makePlaying state.players state.score state.round i

clientSend :: Client n -> Message n -> IO ()
clientSend client msg = case client.state of
  Disconnected _ -> pure ()
  Connected box -> STM.atomically $ STM.writeTMVar box msg

broadcast :: IntMap PlayerId (Client n) -> (PlayerId -> Message n) -> IO ()
broadcast clients makeMsg =
  forM_ clients $ \client -> clientSend client (makeMsg client.player.id)

applyAction :: (KnownNat n) => DB.Pool -> Game n -> Phase n -> Player -> Action n -> IO (Either (Rejection n) (Phase n))
applyAction db game phase player action = trying $ \reject -> case action of
  TakeSeat seat -> case phase of
    Playing {} -> throw reject GameAlreadyStarted
    Waiting seats
      | isJust playerSeat -> throw reject AlreadySeated
      | isJust (Vec.index seats seat) -> throw reject SeatTaken
      | otherwise -> do
          ok <- DB.use db $ Hasql.statement (game.id, player.id, seat) insertSeat
          unless ok $ error "seat insert failed"
          pure $ Waiting $ Vec.set seat (Just player) seats
  StartGame -> case phase of
    Playing {} -> throw reject GameAlreadyStarted
    Waiting seats -> case Vec.sequence seats of
      Nothing -> error "game not full"
      Just players
        | isNothing playerSeat -> throw reject NotAPlayer
        | otherwise -> do
            hands <- case sameNat game (Proxy @4) of
              Nothing -> error "only implemented for 4 players"
              Just Refl -> Random.applyIOGen Dealer.four game.ioGen
            let chooser = 0 -- TODO: choose based on weli
                round = Round.start chooser hands
                gameState =
                  GameState
                    { players,
                      score = Vec.replicate 0,
                      roundNumber = RoundNumber 1,
                      cardSeqNumber = 0,
                      round
                    }
            DB.statement db insertRound (game.id, gameState.roundNumber, chooser, hands)
            pure $ Playing gameState
  Shove -> case phase of
    Waiting {} -> throw reject GameNotStarted
    Playing gs -> do
      checkCurrentPlayer reject gs.round
      case Round.shove gs.round of
        Left shoveError -> throw reject $ ShoveError shoveError
        Right round -> do
          DB.statement db setRoundShoved (game.id, gs.roundNumber)
          pure $ Playing $ gs {round = round, players = gs.players} -- Ambiguous record update
  DeclareVariant variant -> case phase of
    Waiting {} -> throw reject GameNotStarted
    Playing gs -> do
      checkCurrentPlayer reject gs.round
      case Round.choose variant gs.round of
        Left _ -> throw reject VariantAlreadyDeclared
        Right round -> do
          DB.statement db setRoundVariant (game.id, gs.roundNumber, variant)
          pure $ Playing $ gs {round = round, players = gs.players} -- Ambiguous record update
  PlayCard card skip -> case phase of
    Waiting _ -> throw reject GameNotStarted
    Playing gs -> do
      checkCurrentPlayer reject gs.round
      let seat = Round.currentPlayer gs.round
      -- Both steps are pure: a rejected card leaves the Weis untouched.
      skipped <-
        if skip
          then case Round.skipMeld seat gs.round of
            Left Round.AlreadyAnnounced -> throw reject MeldAlreadyAnnounced
            Right r -> pure r
          else pure gs.round
      case Round.playCard card skipped of
        Left playError -> throw reject $ PlayError playError
        Right round -> do
          let cardParams = (game.id, gs.roundNumber, gs.cardSeqNumber, card)
              result = Round.close round
          if not skip && isNothing result
            then DB.statement db insertPlayedCard cardParams
            else DB.transaction db $ do
              Transaction.statement cardParams insertPlayedCard
              when skip $
                Transaction.statement (game.id, gs.roundNumber, seat) insertSkippedMeld
              forM_ result $ \r ->
                Transaction.statement (game.id, gs.roundNumber, Round.total r) setRoundPoints
          pure $ Playing $ gs {round = round, cardSeqNumber = gs.cardSeqNumber + 1}
  NextRound -> case phase of
    Waiting _ -> throw reject GameNotStarted
    Playing gs
      | isNothing playerSeat -> throw reject NotAPlayer
      | otherwise -> case Round.close gs.round of
          Nothing -> throw reject RoundNotFinished
          Just result -> do
            hands <- case sameNat game (Proxy @4) of
              Nothing -> error "only implemented for 4 players"
              Just Refl -> Random.applyIOGen Dealer.four game.ioGen
            let chooser = Round.chooser gs.round + 1
                roundNumber = RoundNumber $ gs.roundNumber.toInt16 + 1
            DB.statement db insertRound (game.id, roundNumber, chooser, hands)
            pure . Playing $
              GameState
                { players = gs.players,
                  score = Vec.zipWith (+) gs.score (Round.total result),
                  roundNumber,
                  cardSeqNumber = 0,
                  round = Round.start chooser hands
                }
  where
    playerSeat = case phase of
      Waiting seats -> Vec.findIndex (maybe False $ \p -> p.id == player.id) seats
      Playing gs -> Vec.findIndex (\p -> p.id == player.id) gs.players
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

insertRound :: forall n. (KnownNat n) => Hasql.Statement (GameId, RoundNumber, Fin n, Vec n CardSet) ()
insertRound =
  lmap
    encode
    [resultlessStatement|
      insert into rounds (game_id, round_number, chooser, initial_hands)
      values ($1::int8, $2::int2, $3::int2, $4::int8[])
    |]
  where
    encode (gameId, roundNumber, chooser, hands) =
      ( Id.toInt64 gameId,
        roundNumber.toInt16,
        fromIntegral (Fin.toInt chooser),
        Vec.toVector (Vec.map CardSet.toInt64 hands) :: VU.Vector Int64
      )

setRoundPoints :: Hasql.Statement (GameId, RoundNumber, Vec 2 Int) ()
setRoundPoints =
  lmap
    encode
    [resultlessStatement|
      update rounds set points = $3::int4[], finished_at = now()
      where game_id = $1::int8 and round_number = $2::int2
    |]
  where
    encode (gameId, roundNumber, points) =
      ( Id.toInt64 gameId,
        roundNumber.toInt16,
        Vec.toVector (Vec.map fromIntegral points) :: VU.Vector Int32
      )

setRoundShoved :: Hasql.Statement (GameId, RoundNumber) ()
setRoundShoved =
  lmap
    (\(gameId, roundNumber) -> (Id.toInt64 gameId, roundNumber.toInt16))
    [resultlessStatement|
      update rounds set shoved = true
      where game_id = $1::int8 and round_number = $2::int2
    |]

setRoundVariant :: Hasql.Statement (GameId, RoundNumber, Variant) ()
setRoundVariant =
  lmap
    encode
    [resultlessStatement|
      update rounds set variant = $3::int2 
      where game_id = $1::int8 and round_number = $2::int2
    |]
  where
    encode (gameId, roundNumber, variant) =
      (Id.toInt64 gameId, roundNumber.toInt16, Variant.toInt16 variant)

insertPlayedCard :: Hasql.Statement (GameId, RoundNumber, Int16, Card) ()
insertPlayedCard =
  lmap
    encode
    [resultlessStatement|
      insert into played_cards (game_id, round_number, seq, card)
      values ($1::int8, $2::int2, $3::int2, $4::int2)
    |]
  where
    encode (gameId, roundNumber, s, card) =
      (Id.toInt64 gameId, roundNumber.toInt16, s, Card.toInt16 card)

-- | Adds the seat to @rounds.skipped_melds@ (a 'BitSet').
insertSkippedMeld :: Hasql.Statement (GameId, RoundNumber, Fin n) ()
insertSkippedMeld =
  lmap
    encode
    [resultlessStatement|
      update rounds set skipped_melds = skipped_melds | $3::int2
      where game_id = $1::int8 and round_number = $2::int2
    |]
  where
    encode (gameId, roundNumber, seat) =
      (Id.toInt64 gameId, roundNumber.toInt16, Bits.bit (Fin.toInt seat))

-- | Generic, except for 'PlayCard': @{"tag": "PlayCard", "card": …}@ with an
-- optional @"skipMeld"@. It is left out when false, so the value of an
-- included checkbox (@"on"@ when checked, missing otherwise) isn't
-- overwritten by a button's @hx-vals@.
instance (KnownNat n) => ToJSON (Action n) where
  toJSON = \case
    PlayCard card skip ->
      Aeson.object $
        ["tag" .= ("PlayCard" :: Text), "card" .= card]
          <> ["skipMeld" .= True | skip]
    action -> Aeson.genericToJSON Aeson.defaultOptions action

instance (KnownNat n) => FromJSON (Action n) where
  parseJSON v = case v of
    Aeson.Object o ->
      o .: "tag" >>= \case
        ("PlayCard" :: Text) -> do
          card <- o .: "card"
          skip <-
            o .:? "skipMeld" >>= \case
              Nothing -> pure False
              Just (Aeson.Bool b) -> pure b
              Just (Aeson.String "on") -> pure True
              Just other -> Aeson.Types.typeMismatch "skipMeld" other
          pure $ PlayCard card skip
        _ -> generic
    _ -> generic
    where
      generic = Aeson.genericParseJSON Aeson.defaultOptions v

note :: Text -> Maybe a -> Either Text a
note msg = maybe (Left msg) pure
