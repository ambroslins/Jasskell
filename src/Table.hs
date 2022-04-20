module Table where

import Action (Action (..))
import Control.Monad.Except (Except, MonadError (..), runExcept)
import Data.Finite (Finite)
import Data.Vector.Sized (Vector)
import Data.Vector.Sized qualified as Vector
import GameState (GameState)
import GameState qualified
import Jass (JassNat)
import Message (Message (..))
import Relude.Extra.Lens (set)
import User (User)
import User qualified
import Prelude hiding (join)

newtype Table n = Table (TVar (TableState n))

data TableState n
  = Waiting (Vector n (Maybe (User n)))
  | Playing (Vector n (User n)) (GameState n)

new :: KnownNat n => STM (Table n)
new = Table <$> newTVar (Waiting $ Vector.replicate Nothing)

join :: JassNat n => Text -> Finite n -> Table n -> STM (Maybe (Action -> STM (), STM (Message n)))
join username seat t@(Table table) =
  readTVar table >>= \case
    Waiting users
      | isJust $ Vector.index users seat -> pure Nothing
      | otherwise -> do
        (user, getMsg) <- User.make username
        writeTVar table $ Waiting $ set (Vector.ix seat) (Just user) users
        pure $ pure (update t seat, getMsg)
    Playing _ _ -> pure Nothing

update :: JassNat n => Table n -> Finite n -> Action -> STM ()
update (Table table) player action = do
  tableState <- readTVar table
  case runExcept $ updateState player action tableState of
    Left e -> pure () -- TODO: send error message to player
    Right ts -> do
      writeTVar table ts

data Error
  = GameNotReady
  | GameError GameState.Error
  deriving (Show)

updateState :: forall n m. (JassNat n, MonadError Error m) => Finite n -> Action -> TableState n -> m (TableState n)
updateState player action tableState = case action of
  PlayCard card -> updateGameState $ GameState.playCard player card
  DeclareVariant variant -> updateGameState $ GameState.declareVariant player variant
  where
    updateGameState :: (GameState n -> Except GameState.Error (GameState n)) -> m (TableState n)
    updateGameState f = case tableState of
      Waiting _ -> throwError GameNotReady
      Playing users gameState -> case runExcept $ f gameState of
        Left e -> throwError $ GameError e
        Right gs -> pure $ Playing users gs