{-# LANGUAGE TemplateHaskell #-}

module Jass where

import Card (Card)
import Card qualified
import Control.Monad.Except
import Control.Monad.Free
import Control.Monad.Free.TH (makeFree)
import Data.Finite (Finite)
import GHC.TypeLits (Div, KnownNat, type (+), type (-))
import Variant (Variant)
import View (Declaring, Playing, View)

data Jass n next
  = PromptCard (Finite n) (View Playing n) (Card -> Except Card.Reason Card) (Card -> next)
  | PromptVariant (Finite n) (View Declaring n) (Variant -> Except () Variant) (Variant -> next)
  deriving (Functor)

makeFree ''Jass

type JassNat n = (KnownNat n, KnownNat (Div 36 n), n ~ ((n - 1) + 1))

type MonadJass n m = (JassNat n, MonadFree (Jass n) m)
