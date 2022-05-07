module Jasskell.Server.GameState where

import Control.Monad.Except (MonadError (..))
import Control.Monad.Free (Free (..))
import Data.Finite (Finite)
import Jasskell.Card (Card)
import Jasskell.Game (Game)
import Jasskell.Jass (Jass (..))
import Jasskell.Variant (Variant)
import Jasskell.View (SomeView (..))
import Jasskell.View.Playing qualified as View.Playing

type GameState n = Free (Jass n) (Game n)

data Error
  = GameAlreadyOver
  | VariantNotDefined
  | VariantAlreadyDefined
  | CardUnplayable View.Playing.Reason
  deriving (Show)

views :: Jass n a -> Finite n -> SomeView n
views = \case
  PromptCard view _ -> ViewPlaying . view
  PromptVariant view _ -> ViewDeclaring . view

withActiveGame :: MonadError Error m => (Jass n (GameState n) -> m (GameState n)) -> GameState n -> m (GameState n)
withActiveGame f = \case
  Pure _ -> throwError GameAlreadyOver
  Free g -> f g

playCard :: (MonadError Error m, KnownNat n) => Finite n -> Card -> GameState n -> m (GameState n)
playCard player card = withActiveGame $ \case
  PromptVariant _ _ -> throwError VariantNotDefined
  PromptCard vs play -> case View.Playing.validateCard (vs player) card of
    Left reason -> throwError $ CardUnplayable reason
    Right validCard -> pure $ play validCard

declareVariant :: MonadError Error m => Finite n -> Variant -> GameState n -> m (GameState n)
declareVariant _ variant = withActiveGame $ \case
  PromptCard _ _ -> throwError VariantAlreadyDefined
  PromptVariant _ play -> pure $ play variant
