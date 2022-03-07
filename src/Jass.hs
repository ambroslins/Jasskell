{-# LANGUAGE TemplateHaskell #-}

module Jass where

import Action (Action)
import Card (Card, Cards)
import Card qualified
import Control.Monad.Except
import Control.Monad.Free
import Control.Monad.Free.TH (makeFree)
import Data.Finite (Finite)
import Data.Vector.Sized (Vector)
import JassNat (JassNat)
import Variant (Variant)

data GameView n = GameView
  { trick :: Vector n (Maybe Card),
    variant :: Maybe Variant,
    hand :: Cards
  }
  deriving (Show)

type MonadJass n m = (JassNat n, MonadFree (Jass n) m)

data Error
  = CardUnplayable Card.Reason
  | VariantAlreadyDefined
  | VariantNotDefined
  deriving (Show)

data Jass n next
  = Prompt (Finite n -> GameView n) (Finite n) (Action -> next)
  | ReportError (Finite n) (Error) next
  deriving (Functor)

makeFree ''Jass

withPlayerAction :: MonadFree (Jass n) m => (Finite n -> GameView n) -> Finite n -> (Action -> ExceptT Error m a) -> m a
withPlayerAction view player m = do
  action <- prompt view player
  result <- runExceptT $ m action
  case result of
    Right a -> pure a
    Left e -> do
      reportError player e
      withPlayerAction view player m
