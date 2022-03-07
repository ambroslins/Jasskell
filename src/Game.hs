module Game where

import Action
import Card qualified
import Control.Monad.Except
import Data.Finite (Finite)
import JassNat (JassNat)
import Round qualified
import Prelude hiding (round)

data Game n = Game
  { round :: SomeRound n,
    previousRounds :: [Round.Finished n],
    settings :: Settings
  }
  deriving (Show)

data SomeRound n
  = Starting (Round.Starting n)
  | Playing (Round.Playing n)
  deriving (Show)

data Settings = Settings
  deriving (Show)

data Error
  = CardUnplayable Card.Reason
  | VariantNotDefined
  | VariantAlreadyDefined
  | NotYourTurn
  deriving (Show)

update :: (MonadError Error m, JassNat n) => Finite n -> Action -> Game n -> m (Game n)
update player action game = do
  liftEither <=< runExceptT $ case action of
    PlayCard card -> case game.round of
      Starting _ -> throwError VariantNotDefined
      Playing round ->
        if player /= Round.current round
          then throwError NotYourTurn
          else
            withExceptT CardUnplayable $
              updateRound . either Playing undefined <$> Round.playCard card round
    ChooseVariant variant -> case game.round of
      Playing _ -> throwError VariantAlreadyDefined
      Starting round ->
        pure . updateRound . Playing $
          Round.chooseVariant variant round
  where
    updateRound round = game {round = round}