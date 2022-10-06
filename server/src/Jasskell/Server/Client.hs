module Jasskell.Server.Client
  ( Client,
    make,
    send,
    recive,
  )
where

import Control.Concurrent.STM.TMVar (writeTMVar)
import Data.Unique (Unique, newUnique)
import Jasskell.Server.Message (Message)

data Client n = Client
  { messageVar :: TMVar (Message n),
    unique :: Unique
  }

instance Eq (Client n) where
  (==) = (==) `on` unique

instance Hashable (Client n) where
  hashWithSalt seed = hashWithSalt seed . unique

make :: MonadIO m => m (Client n)
make = Client <$> newEmptyTMVarIO <*> liftIO newUnique

send :: Client n -> Message n -> STM ()
send = writeTMVar . messageVar

recive :: Client n -> STM (Message n)
recive = takeTMVar . messageVar
