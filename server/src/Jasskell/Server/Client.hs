module Jasskell.Server.Client
  ( Client,
    ClientID,
    make,
    send,
    recive,
  )
where

import Control.Concurrent.STM.TMVar (writeTMVar)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Jasskell.Server.Message (Message)

newtype ClientID = ClientID UUID
  deriving newtype (Eq, Ord, Hashable, Show)

newtype Client n = Client {messageVar :: TMVar (Message n)}

make :: MonadIO m => m (ClientID, Client n)
make = do
  clientID <- ClientID <$> liftIO nextRandom
  client <- Client <$> newEmptyTMVarIO
  pure (clientID, client)

send :: Client n -> Message n -> STM ()
send = writeTMVar . messageVar

recive :: Client n -> STM (Message n)
recive = takeTMVar . messageVar
