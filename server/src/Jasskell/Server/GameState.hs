{-# LANGUAGE TemplateHaskell #-}

module Jasskell.Server.GameState
  ( GameState,
    Transition (..),
    Move (..),
    BadMove (..),
    View (..),
    start,
    update,
    views,
  )
where

import Control.Monad.Except (MonadError (..), liftEither)
import Control.Monad.Free (MonadFree, liftF)
import Control.Monad.Free.TH (makeFree)
import Control.Monad.Trans.Free (FreeF (..), FreeT (runFreeT))
import Data.Finite (Finite)
import Jasskell.Card (BadCard, Card)
import Jasskell.Declaration (BadDeclaration, Declaration)
import Jasskell.Game (Game)
import Jasskell.Game qualified as Game
import Jasskell.Jass (JassNat)
import Jasskell.View.Declaring (Declaring)
import Jasskell.View.Playing (Playing)
import Jasskell.Views (Views)
import Jasskell.Views qualified as Views

data PromptF n next
  = PromptCard (Finite n) (Views Playing n) (Card -> next)
  | PromptDeclaration (Finite n) (Views Declaring n) (Declaration -> next)
  deriving (Functor)

makeFree ''PromptF

newtype GameState n = GameState (PromptF n (FreeT (PromptF n) (Either BadMove) (Game n)))

data Transition n
  = Continue (GameState n)
  | Done (Game n)

data Move
  = PlayCard Card
  | Declare Declaration
  deriving (Eq, Show)

data BadMove
  = NotYourTurn
  | VariantNotDefined
  | VariantAlreadyDefined
  | BadCard BadCard
  | BadDeclaration BadDeclaration
  deriving (Eq, Show)

data View n
  = Playing (Playing n)
  | Declaring (Declaring n)
  deriving (Eq, Show)

update ::
  forall n m.
  MonadError BadMove m =>
  Finite n ->
  Move ->
  GameState n ->
  m (Transition n)
update player action (GameState prompt) = transition $ case prompt of
  PromptCard current _ play -> do
    when (player /= current) $ throwError NotYourTurn
    case action of
      Declare _ -> throwError VariantAlreadyDefined
      PlayCard card -> play card
  PromptDeclaration current _ play -> do
    when (player /= current) $ throwError NotYourTurn
    case action of
      PlayCard _ -> throwError VariantNotDefined
      Declare declaration -> play declaration

transition :: MonadError BadMove m => FreeT (PromptF n) (Either BadMove) (Game n) -> m (Transition n)
transition f =
  liftEither $
    runFreeT f <&> \case
      Pure game -> Done game
      Free play -> Continue $ GameState play

start :: (JassNat n, MonadError BadMove m) => m (Transition n)
start =
  transition $
    Game.play $
      Game.Interface
        { Game.promptCard = promptCard,
          Game.promptDeclaration = promptDeclaration,
          Game.deal = undefined,
          Game.throwBadCard = throwError . BadCard,
          Game.throwBadDeclaration = throwError . BadDeclaration
        }

views :: GameState n -> Views View n
views (GameState f) = case f of
  PromptCard _ vs _ -> Views.map Playing vs
  PromptDeclaration _ vs _ -> Views.map Declaring vs
