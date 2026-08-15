{-# LANGUAGE ViewPatterns #-}

module Jasskell.App
  ( Env (..),
    AppT,
    runAppT,
    hoistAppT,
    useDB,
    useDBCatch,
  )
where

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader
  ( MonadIO (liftIO),
    MonadReader (ask),
    ReaderT (..),
    asks,
    runReaderT,
  )
import Control.Monad.Trans (MonadTrans (..))
import Crypto.Random (MonadRandom (..))
import Data.Coerce (coerce)
import Data.Text (Text)
import Hasql.Errors
  ( ServerError (..),
    SessionError (..),
    StatementError (..),
  )
import Hasql.Pool (UsageError (..))
import Hasql.Session qualified as Hasql
import Jasskell.Database qualified as DB
import Jasskell.Logger (Logger, MonadLogger (..))
import UnliftIO (throwIO)

data Env = Env
  { db :: DB.Pool,
    logger :: Logger
  }

newtype AppT m a = AppT (ReaderT Env m a)
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadUnliftIO,
      MonadReader Env,
      MonadError e
    )

instance (MonadIO m) => MonadRandom (AppT m) where
  getRandomBytes = liftIO . getRandomBytes

instance (Monad m) => MonadLogger (AppT m) where
  askLogger = asks (.logger)

instance MonadTrans AppT where
  lift = AppT . lift

runAppT :: Env -> AppT m a -> m a
runAppT env (AppT m) = runReaderT m env

hoistAppT :: (m a -> n b) -> AppT m a -> AppT n b
hoistAppT hoist (AppT (ReaderT m)) = coerce $ hoist . m

useDB :: (MonadIO m) => Hasql.Session a -> AppT m a
useDB session = do
  env <- ask
  DB.use throwIO env.db session

-- | Execute a 'Hasql.Session' using a connection from the pool and catch
-- the SQLSTATE error.
useDBCatch :: (MonadIO m) => (Text -> Maybe a) -> Hasql.Session a -> AppT m a
useDBCatch onError session = do
  env <- ask
  DB.use onUsageError env.db session
  where
    onUsageError = \case
      SessionUsageError (StatementSessionError _ _ _ _ _ (ServerStatementError (ServerError (onError -> Just a) _ _ _ _))) -> pure a
      e -> throwIO e
