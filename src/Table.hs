module Table where

import Action (Action (..))
import Card (Card)
import Card.Valid qualified as Card
import Control.Monad.Except (MonadError (..), runExcept)
import Control.Monad.Free (Free (..))
import Data.Finite (Finite)
import Data.Vector.Sized (Vector)
import Data.Vector.Sized qualified as Vector
import Game (Game)
import Jass (Jass (..), JassNat)
import Message (Message (..))
import Relude.Extra.Lens (set)
import User (User)
import User qualified
import Variant (Variant)
import Prelude hiding (join)

newtype Table n = Table (TVar (TableState n))

data TableState n
  = Setup (Vector n (Maybe (User n)))
  | Playing (Vector n (User n)) (GameState n)

type GameState n = Jass n (Free (Jass n) (Game n))

new :: KnownNat n => STM (Table n)
new = Table <$> newTVar (Setup $ Vector.replicate Nothing)

join :: JassNat n => Text -> Finite n -> Table n -> STM (Maybe (Action -> STM (), STM (Message n)))
join username seat t@(Table table) =
  readTVar table >>= \case
    Setup users
      | isJust $ Vector.index users seat -> pure Nothing
      | otherwise -> do
        (user, getMsg) <- User.make username
        writeTVar table $ Setup $ set (Vector.ix seat) (Just user) users
        pure $ pure (update t seat, getMsg)
    Playing _ _ -> pure Nothing

update :: JassNat n => Table n -> Finite n -> Action -> STM ()
update (Table table) player action = do
  tableState <- readTVar table
  case runExcept $ updateState player action tableState of
    Left _ -> pure () -- TODO: send error message to player
    Right ts -> do
      writeTVar table ts -- TODO: send updated view to player

data Error
  = NoActiveGame
  | VariantNotDefined
  | VariantAlreadyDefined
  | CardUnplayable Card.Reason
  deriving (Eq, Show)

updateState :: forall n m. (JassNat n, MonadError Error m) => Finite n -> Action -> TableState n -> m (TableState n)
updateState player action tableState = case action of
  PlayCard card -> playCard card
  DeclareVariant variant -> declareVariant variant
  where
    withGameState :: (GameState n -> m (Free (Jass n) (Game n))) -> m (TableState n)
    withGameState f = case tableState of
      Setup _ -> throwError NoActiveGame
      Playing users gameState ->
        f gameState <&> \case
          Pure _ -> Setup (Vector.map Just users)
          Free g -> Playing users g

    playCard :: Card -> m (TableState n)
    playCard card = withGameState $ \case
      PromptVariant _ _ -> throwError VariantNotDefined
      PromptCard views play -> case Card.validate (views player) card of
        Left reason -> throwError $ CardUnplayable reason
        Right validCard -> pure $ play validCard

    declareVariant :: Variant -> m (TableState n)
    declareVariant variant = withGameState $ \case
      PromptCard _ _ -> throwError VariantAlreadyDefined
      PromptVariant _ play -> pure $ play variant
