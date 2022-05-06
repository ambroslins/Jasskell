module ServerState where

import Control.Concurrent.STM.TVar (modifyTVar)
import Data.HashMap.Strict qualified as HashMap
import Jass (JassNat)
import Table (Table, TableID)
import Table qualified

data SomeTable = forall n. JassNat n => SomeTable (Table n)

newtype ServerState = ServerState {tables :: TVar (HashMap TableID SomeTable)}

empty :: IO ServerState
empty = ServerState <$> newTVarIO mempty

createTable :: forall n. JassNat n => ServerState -> IO (TableID, Table n)
createTable ServerState {tables} = do
  (tableID, table) <- Table.new @n
  atomically $ modifyTVar tables (HashMap.insert tableID (SomeTable table))
  pure (tableID, table)

lookupTable :: ServerState -> TableID -> STM (Maybe SomeTable)
lookupTable ServerState {tables} tableID = HashMap.lookup tableID <$> readTVar tables