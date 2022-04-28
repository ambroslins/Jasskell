module GameState where

import Card (Card)
import Control.Monad.Except (MonadError (..))
import Control.Monad.Free (Free (..))
import Data.Finite (Finite)
import Game (Game)
import Jass (Jass (..))
import Variant (Variant)
import View.Playing qualified

type GameState n = Free (Jass n) (Game n)

data Error
  = GameAlreadyOver
  | VariantNotDefined
  | VariantAlreadyDefined
  | CardUnplayable View.Playing.Reason
  deriving (Show)

withActiveGame :: MonadError Error m => (Jass n (Free (Jass n) (Game n)) -> m (GameState n)) -> GameState n -> m (GameState n)
withActiveGame f = \case
  Pure _ -> throwError GameAlreadyOver
  Free g -> f g

playCard :: (MonadError Error m, KnownNat n) => Finite n -> Card -> GameState n -> m (GameState n)
playCard player card = withActiveGame $ \case
  PromptVariant _ _ -> throwError VariantNotDefined
  PromptCard views play -> case View.Playing.validateCard (views player) card of
    Left reason -> throwError $ CardUnplayable reason
    Right validCard -> pure $ play validCard

declareVariant :: MonadError Error m => Finite n -> Variant -> GameState n -> m (GameState n)
declareVariant _ variant = withActiveGame $ \case
  PromptCard _ _ -> throwError VariantAlreadyDefined
  PromptVariant _ play -> pure $ play variant
