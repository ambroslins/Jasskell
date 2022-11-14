module Jasskell.Server.Client
  ( Client,
    make,
    sendMessage,
  )
where

import Data.Unique (Unique, newUnique)
import Jasskell.Server.Action (Action)
import Jasskell.Server.Message (Message)

data Client n = Client
  { sendMsg :: Message n -> IO (),
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
make send sendAction = do
  client <- Client send <$> liftIO newUnique
  pure (client, atomically . sendAction client)

sendMessage :: MonadIO m => Client n -> Message n -> m ()
sendMessage client = liftIO . sendMsg client
