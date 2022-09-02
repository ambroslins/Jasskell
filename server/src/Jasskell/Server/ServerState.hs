module Jasskell.Server.ServerState where

import Control.Concurrent.STM.TVar (modifyTVar)
import Data.HashMap.Strict qualified as HashMap
import Jasskell.Dealer qualified as Dealer
import Jasskell.Jass (JassNat)
import Jasskell.Server.Table (Table)
import Jasskell.Server.Table qualified as Table
import Jasskell.Server.TableID (TableID)

data SomeTable = forall n. JassNat n => SomeTable (Table n)

newtype ServerState = ServerState {tables :: TVar (HashMap TableID SomeTable)}

empty :: IO ServerState
empty = ServerState <$> newTVarIO mempty

createTable :: ServerState -> IO (TableID, Table 4)
createTable ServerState {tables} = do
  (tableID, table) <- Table.new Dealer.four
  atomically $ modifyTVar tables (HashMap.insert tableID (SomeTable table))
  pure (tableID, table)

lookupTable :: ServerState -> TableID -> STM (Maybe SomeTable)
lookupTable ServerState {tables} tableID =
  HashMap.lookup tableID <$> readTVar tables