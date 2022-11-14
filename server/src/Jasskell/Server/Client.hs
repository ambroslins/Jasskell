module Jasskell.Server.Client
  ( Client,
    make,
    sendMessage,
  )
where

import Colog (WithLog, logWarning)
import Colog qualified
import Control.Concurrent.STM (writeTMVar)
import Data.Unique (Unique, newUnique)
import Jasskell.Server.Action (Action)
import Jasskell.Server.Message (Message)
import UnliftIO (MonadUnliftIO, catchAny, throwIO)

data Client n = Client
  { sendMsg :: Message n -> IO (),
    exception :: TMVar SomeException,
    clientID :: Unique
  }

instance Eq (Client n) where
  (==) = (==) `on` clientID

instance Hashable (Client n) where
  hashWithSalt seed = hashWithSalt seed . clientID

make ::
  MonadIO m =>
  (Message n -> IO ()) ->
  (Client n -> Action n -> STM ()) ->
  m (Client n, Action n -> IO ())
make send sendClientAction = do
  client <- Client send <$> newEmptyTMVarIO <*> liftIO newUnique
  let sendAction action =
        atomically (tryReadTMVar (exception client))
          >>= \case
            Nothing -> atomically (sendClientAction client action)
            Just (SomeException e) -> throwIO e

  pure (client, sendAction)

sendMessage ::
  (WithLog env Colog.Message m, MonadUnliftIO m) =>
  Client n ->
  Message n ->
  m ()
sendMessage client message =
  liftIO (sendMsg client message)
    `catchAny` \e -> do
      logWarning ("Client.sendMessage threw exception: " <> show e)
      atomically (writeTMVar (exception client) e)
