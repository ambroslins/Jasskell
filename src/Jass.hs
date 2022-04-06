{-# LANGUAGE TemplateHaskell #-}

module Jass where

import Card (Card)
import Control.Monad.Free (MonadFree, liftF)
import Control.Monad.Free.TH (makeFree)
import GHC.TypeLits (Div, type (+), type (-))
import Variant (Variant)

data View :: Nat -> Type

data Jass n next
  = PromptCard (View n) (Card -> next)
  | PromptVariant (View n) (Variant -> next)
  deriving (Functor)

makeFree ''Jass

type MonadJass n m = (JassNat n, MonadFree (Jass n) m)

type JassNat n = (KnownNat n, KnownNat (Div 36 n), n ~ ((n - 1) + 1))