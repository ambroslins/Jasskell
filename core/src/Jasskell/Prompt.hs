{-# LANGUAGE TemplateHaskell #-}

module Jasskell.Prompt
  ( Prompt (..),
    card,
    variant,
    mapView,
  )
where

import Control.Monad.Free (MonadFree, liftF)
import Control.Monad.Free.TH (makeFree)
import Control.Monad.Trans.Free (FreeT, iterT)
import Jasskell.Card (Card)
import Jasskell.Variant (Variant)

data Prompt view next
  = Card view (Card -> next)
  | Variant view (Variant -> next)
  deriving (Functor)

makeFree ''Prompt

mapView ::
  MonadFree (Prompt view') m =>
  (view -> view') ->
  FreeT (Prompt view) m a ->
  m a
mapView f = iterT $ \case
  Card view next -> card (f view) >>= next
  Variant view next -> variant (f view) >>= next
