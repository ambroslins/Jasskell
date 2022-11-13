module Jasskell.Server.Client
  ( Client,
    make,
    sendMessage,
    reciveAction,
  )
where

import Control.Concurrent.STM.TMVar (writeTMVar)
import Data.Unique (Unique, newUnique)
import GHC.Show (Show (..))
import Jasskell.Server.Action (Action)
import Jasskell.Server.Message (Message)

data Client n = Client
  { send :: Message n -> IO (),
    actionVar :: TMVar (Action n),
    unique :: Unique
  }

instance Eq (Client n) where
  (==) = (==) `on` unique

instance Hashable (Client n) where
  hashWithSalt seed = hashWithSalt seed . unique

instance Show (Client n) where
  show = const "Client"

make :: MonadIO m => (Message n -> IO ()) -> m (Client n, Action n -> IO ())
make s = do
  var <- newEmptyTMVarIO
  client <- Client s var <$> liftIO newUnique
  pure (client, atomically . writeTMVar var)

sendMessage :: MonadIO m => Client n -> Message n -> m ()
sendMessage client = liftIO . send client

reciveAction :: Client n -> STM (Action n)
reciveAction = takeTMVar . actionVar
