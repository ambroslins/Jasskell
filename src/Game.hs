module Game where

import Action
import Card qualified
import Control.Monad.Except
import Data.Finite (Finite)
import JassNat (JassNat)
import Round (Round, RoundFinished)
import Round qualified
import Prelude hiding (round)

data Game n = Game
  { round :: Round n,
    previousRounds :: [RoundFinished n],
    settings :: Settings
  }
  deriving (Show)

data Settings = Settings
  deriving (Show)

data Error
  = CardUnplayable Card.Reason
  | VariantNotDefined
  | VariantAlreadyDefined
  | AlreadyFinished
  | NotYourTurn
  deriving (Show)

update :: JassNat n => Finite n -> Action -> Game n -> Except Error (Game n)
update player action game =
  case action of
    PlayCard card -> case game.round of
      Round.Starting _ -> throwError VariantNotDefined
      Round.Finished _ -> throwError AlreadyFinished
      Round.Playing round ->
        if player /= Round.current round
          then throwError NotYourTurn
          else
            withExcept CardUnplayable $
              updateRound <$> Round.playCard card round
    ChooseVariant variant -> case game.round of
      Round.Playing _ -> throwError VariantAlreadyDefined
      Round.Finished _ -> throwError AlreadyFinished
      Round.Starting round ->
        pure . updateRound . Round.Playing $
          Round.chooseVariant variant round
  where
    updateRound round = game {round = round}