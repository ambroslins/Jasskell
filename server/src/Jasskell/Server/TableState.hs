module Jasskell.Server.TableState
  ( TableState,
    make,
    addGuest,
    clients,
    players,
    guests,
    broadcast,
    playerViews,
    guestView,
    applyAction,
  )
where

import Control.Monad.Except (MonadError, liftEither, throwError)
import Data.Finite (Finite)
import Data.HashSet qualified as HashSet
import Data.Vector.Sized (Vector)
import Data.Vector.Sized qualified as Vector
import Jasskell.Dealer (Dealer)
import Jasskell.Game (Game)
import Jasskell.Jass (JassNat)
import Jasskell.Server.Action (Action (..))
import Jasskell.Server.Client (Client)
import Jasskell.Server.Client qualified as Client
import Jasskell.Server.Error (Error (..))
import Jasskell.Server.GameState (BadMove, GameState, Transition (..))
import Jasskell.Server.GameState qualified as GameState
import Jasskell.Server.GuestView (GuestView)
import Jasskell.Server.GuestView qualified as GuestView
import Jasskell.Server.Message qualified as Message
import Jasskell.Server.Player (Player)
import Jasskell.Server.Player qualified as Player
import Jasskell.Server.PlayerView (PlayerView)
import Jasskell.Server.PlayerView qualified as PlayerView
import Jasskell.Server.Seat (Seat (..))
import Jasskell.Server.User (User (User))
import Jasskell.Views (Views)
import Jasskell.Views qualified as Views
import System.Random (StdGen, newStdGen, split)

data TableState n = TableState
  { guests :: HashSet (Client n),
    players :: Vector n (Maybe (Player n)),
    stdGen :: StdGen,
    dealer :: Dealer n,
    phase :: Phase n
  }

data Phase n
  = Waiting
  | Playing (GameState n)
  | Over (Game n)

make :: (KnownNat n, MonadIO m) => Dealer n -> m (TableState n)
make d = do
  gen <- newStdGen
  pure
    TableState
      { guests = HashSet.empty,
        players = Vector.replicate Nothing,
        stdGen = gen,
        dealer = d,
        phase = Waiting
      }

clients :: TableState n -> [Client n]
clients TableState {players, guests} = ps ++ gs
  where
    ps = mapMaybe (fmap Player.client) (Vector.toList players)
    gs = HashSet.toList guests

addGuest :: Client n -> TableState n -> TableState n
addGuest client tableState =
  tableState
    { guests = HashSet.insert client (guests tableState)
    }

findPlayer :: Client n -> TableState n -> Maybe (Finite n)
findPlayer client =
  Vector.findIndex
    (maybe False (\p -> Player.client p == client))
    . players

seats :: TableState n -> Vector n Seat
seats = Vector.map (maybe Empty (Taken . Player.user)) . players

playerViews :: KnownNat n => TableState n -> Views PlayerView n
playerViews tableState = case phase tableState of
  Waiting -> PlayerView.makeWaiting ss
  Playing gameState -> PlayerView.makeActive ss (GameState.views gameState)
  Over game -> PlayerView.makeOver ss game
  where
    ss = seats tableState

guestView :: TableState n -> GuestView n
guestView tableState = case phase tableState of
  Waiting -> GuestView.makeJoining ss
  Playing _ -> GuestView.makeSpectating ss
  Over _ -> GuestView.makeSpectating ss
  where
    ss = seats tableState

broadcast :: (KnownNat n, MonadIO m) => TableState n -> m ()
broadcast tableState = do
  let sendPlayer i =
        maybe
          pass
          ( \p ->
              Client.sendMessage
                (Player.client p)
                (Message.UpdatePlayerView $ Views.pov (playerViews tableState) i)
          )
      sendGuest c =
        Client.sendMessage
          c
          (Message.UpdateGuestView (guestView tableState))
  Vector.imapM_ sendPlayer (players tableState)
  traverse_ sendGuest (guests tableState)

applyAction ::
  (JassNat n, MonadError Error m) =>
  Client n ->
  Action n ->
  TableState n ->
  m (TableState n)
applyAction client action tableState = case action of
  TakeSeat username index -> do
    when (isJust $ findPlayer client tableState) (throwError AlreadySeated)
    let sit s = guard (isNothing s) $> Just (Player.make (User username) client)
    case Vector.ix index sit (players tableState) of
      Nothing -> throwError SeatAlreadyTaken
      Just ps ->
        pure $
          tableState
            { players = ps,
              guests = HashSet.delete client (guests tableState)
            }
  PlayMove move -> case phase tableState of
    Waiting -> throwError WaitingForPlayers
    Over _ -> throwError GameOver
    Playing gameState -> do
      player <-
        whenNothing
          (findPlayer client tableState)
          (throwError NotAPlayer)
      p <- liftEither $ fromTransition (GameState.update player move gameState)
      pure (tableState {phase = p})
  StartGame -> case phase tableState of
    Playing _ -> throwError GameAlreadyStarted
    _ -> do
      let (g, g') = split (stdGen tableState)
      p <- liftEither $ fromTransition $ GameState.start (dealer tableState) g
      pure (tableState {phase = p, stdGen = g'})

fromTransition :: Either BadMove (Transition n) -> Either Error (Phase n)
fromTransition = bimap BadMove $ \case
  Continue gameState -> Playing gameState
  Done game -> Over game
