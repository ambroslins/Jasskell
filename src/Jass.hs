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
  = forall a. Prompt (Finite n) (Finite n -> GameView n) (Action -> Except Error a) (a -> next)

deriving instance Functor (Jass n)

makeFree ''Jass
