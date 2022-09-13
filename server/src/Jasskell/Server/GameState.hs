{-# LANGUAGE TemplateHaskell #-}

module Jasskell.Server.GameState
  ( GameState,
    Transition (..),
    Move (..),
    BadMove (..),
    SomeView (..),
    start,
    update,
    viewPhase,
  )
where

import Control.Monad.Except (MonadError (..), liftEither)
import Control.Monad.Free (MonadFree, liftF)
import Control.Monad.Free.TH (makeFree)
import Control.Monad.Trans.Free (FreeF (..), FreeT (runFreeT))
import Data.Finite (Finite)
import Jasskell.Card (BadCard, Card)
import Jasskell.Dealer (Dealer)
import Jasskell.Dealer qualified as Dealer
import Jasskell.Declaration (BadDeclaration, Declaration)
import Jasskell.Game (Game)
import Jasskell.Game qualified as Game
import Jasskell.Jass (JassNat)
import Jasskell.Server.View qualified as View
import Jasskell.View.Declaring (ViewDeclaring)
import Jasskell.View.Playing (ViewPlaying)
import Jasskell.Views (Views)
import Jasskell.Views qualified as Views
import System.Random (RandomGen)

data PromptF n next
  = PromptCard (Finite n) (Views ViewPlaying n) (Card -> next)
  | PromptDeclaration (Finite n) (Views ViewDeclaring n) (Declaration -> next)
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

data SomeView n
  = Playing (ViewPlaying n)
  | Declaring (ViewDeclaring n)
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

start ::
  (JassNat n, MonadError BadMove m, RandomGen g) =>
  Dealer n ->
  g ->
  m (Transition n)
start dealer = transition . evalStateT game
  where
    game =
      Game.play $
        Game.Interface
          { Game.promptCard = promptCard,
            Game.promptDeclaration = promptDeclaration,
            Game.deal = state (Dealer.run dealer),
            Game.throwBadCard = throwError . BadCard,
            Game.throwBadDeclaration = throwError . BadDeclaration
          }

viewPhase :: GameState n -> Views View.Phase n
viewPhase (GameState f) = case f of
  PromptCard _ vs _ -> Views.map View.Playing vs
  PromptDeclaration _ vs _ -> Views.map View.Declaring vs
