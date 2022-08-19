module Jasskell.Server.Table where

import Control.Monad.Except (Except, MonadError (..), runExcept)
import Control.Monad.Free (Free (..))
import Data.Finite (Finite)
import Data.Vector.Sized (Vector)
import Data.Vector.Sized qualified as Vector
import Jasskell.Jass (JassNat)
import Jasskell.Server.Action (Action (..))
import Jasskell.Server.GameState (GameState)
import Jasskell.Server.GameState qualified as GameState
import Jasskell.Server.Message (Message (..))
import Jasskell.Server.TableID (TableID)
import Jasskell.Server.TableID qualified as TableID
import Jasskell.Server.User (User)
import Jasskell.Server.User qualified as User
import Relude.Extra.Lens (set)
import Prelude hiding (join)

newtype Table n = Table (TVar (TableState n))

data TableState n
  = Waiting (Vector n (Maybe (User n)))
  | Playing (Vector n (User n)) (GameState n)

new :: KnownNat n => IO (TableID, Table n)
new = do
  table <- Table <$> newTVarIO (Waiting $ Vector.replicate Nothing)
  tableID <- TableID.new
  pure (tableID, table)

data Connection n = Connection
  { putAction :: Action -> STM (Either Error ()),
    getMessage :: STM (Message n)
  }

join :: JassNat n => Text -> Finite n -> Table n -> STM (Maybe (Connection n))
join username index t@(Table table) =
  readTVar table >>= \case
    Waiting users
      | isJust $ Vector.index users index -> pure Nothing
      | otherwise -> do
        (user, getMsg) <- User.make username
        messageUsers (\i -> UserJoined username (i - index)) t
        writeTVar table $ Waiting $ set (Vector.ix index) (Just user) users
        pure $
          pure $
            Connection
              { putAction = update t index,
                getMessage = getMsg
              }
    Playing _ _ -> pure Nothing

update :: JassNat n => Table n -> Finite n -> Action -> STM (Either Error ())
update t@(Table table) player action = do
  tableState <- readTVar table
  case runExcept $ updateState player action tableState of
    Left e -> pure $ throwError e
    Right (ts, messages) -> do
      writeTVar table ts
      pure <$> messageUsers messages t

messageUsers :: (Finite n -> Message n) -> Table n -> STM ()
messageUsers messages (Table table) =
  readTVar table >>= \case
    Waiting users -> Vector.imapM_ (maybe pass . messageUser) users
    Playing users _ -> Vector.imapM_ messageUser users
  where
    messageUser i user = User.sendMessage user (messages i)

data Error
  = GameNotReady
  | GameError GameState.Error
  deriving (Show)

updateState :: forall n m. (JassNat n, MonadError Error m) => Finite n -> Action -> TableState n -> m (TableState n, Finite n -> Message n)
updateState player action tableState = case action of
  PlayCard card -> updateGameState $ GameState.playCard player card
  DeclareVariant variant -> updateGameState $ GameState.declareVariant player variant
  where
    updateGameState :: (GameState n -> Except GameState.Error (GameState n)) -> m (TableState n, Finite n -> Message n)
    updateGameState f = case tableState of
      Waiting _ -> throwError GameNotReady
      Playing users gameState -> case runExcept $ f gameState of
        Left e -> throwError $ GameError e
        Right newGameState ->
          pure
            ( Playing users newGameState,
              case newGameState of
                Pure game -> const $ GameOver game
                Free jass -> UpdateView . GameState.views jass
            )