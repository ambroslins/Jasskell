module Jasskell.Server.Env where

import Control.Concurrent.STM.TVar (modifyTVar)
import Data.HashMap.Strict qualified as HashMap
import Jasskell.Dealer qualified as Dealer
import Jasskell.Server.Table (SomeTable (..))
import Jasskell.Server.Table qualified as Table
import Jasskell.Server.TableID (TableID)

newtype Env = Env
  { tables :: TVar (HashMap TableID SomeTable)
  }

init :: IO Env
init = Env <$> newTVarIO HashMap.empty

createTable :: (MonadReader Env m, MonadIO m) => m TableID
createTable = do
  Env {tables} <- ask
  liftIO $ do
    (tableID, table) <- Table.new Dealer.four
    atomically $ modifyTVar tables (HashMap.insert tableID (SomeTable table))
    putStrLn $ "Created new table: " <> show tableID
    pure tableID

lookupTable :: (MonadReader Env m, MonadIO m) => TableID -> m (Maybe SomeTable)
lookupTable tableID = do
  Env {tables} <- ask
  atomically $ HashMap.lookup tableID <$> readTVar tables