module Jasskell.Server.App
  ( AppT,
    MonadApp,
    Env,
    runAppT,
    createTable,
    lookupTable,
    makeEnv,
    hoistEnv,
  )
where

import Colog.Core
  ( HasLog (getLogAction, setLogAction),
    LogAction,
    hoistLogAction,
  )
import Colog.Message (logInfo)
import Colog.Message qualified as Colog
import Control.Concurrent.STM.TVar (modifyTVar)
import Control.Monad.Except (MonadError)
import Data.HashMap.Strict qualified as HashMap
import Jasskell.Dealer qualified as Dealer
import Jasskell.Server.Table (SomeTable (..))
import Jasskell.Server.Table qualified as Table
import Jasskell.Server.TableID (TableID)
import Jasskell.Server.TableID qualified as TableID

newtype AppT m a = AppT (ReaderT (Env (AppT m)) m a)
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader (Env (AppT m)),
      MonadError e
    )

type MonadApp m = MonadReader (Env m) m

instance MonadTrans AppT where
  lift m = AppT (lift m)

runAppT :: Monad m => Env m -> AppT m a -> m a
runAppT env (AppT m) = runReaderT m (liftEnv env)

data Env m = Env
  { tables :: TVar (HashMap TableID SomeTable),
    logAction :: LogAction m Colog.Message
  }

instance HasLog (Env m) Colog.Message m where
  getLogAction = logAction
  setLogAction logAction env = env {logAction}

liftEnv :: (Monad m, MonadTrans t) => Env m -> Env (t m)
liftEnv = hoistEnv lift

hoistEnv :: (forall x. m x -> n x) -> Env m -> Env n
hoistEnv f env = env {logAction = hoistLogAction f (logAction env)}

makeEnv :: MonadIO m => LogAction m Colog.Message -> m (Env m)
makeEnv logAction = do
  tables <- newTVarIO HashMap.empty
  pure Env {tables, logAction}

createTable :: (MonadApp m, MonadIO m) => m TableID
createTable = do
  Env {tables} <- ask
  tableID <- TableID.new
  table <- Table.new Dealer.four
  atomically $ modifyTVar tables (HashMap.insert tableID (SomeTable table))
  logInfo $ "Created new table: " <> show tableID
  pure tableID

lookupTable :: (MonadApp m, MonadIO m) => TableID -> m (Maybe SomeTable)
lookupTable tableID = do
  Env {tables} <- ask
  atomically $ HashMap.lookup tableID <$> readTVar tables