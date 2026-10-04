module Jasskell.App
  ( Env (..),
    AppT,
    runAppT,
    hoistAppT,
  )
where

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader
  ( MonadIO (liftIO),
    MonadReader,
    ReaderT (..),
    asks,
    runReaderT,
  )
import Control.Monad.Trans (MonadTrans (..))
import Crypto.Random (MonadRandom (..))
import Data.Coerce (coerce)
import Jasskell.Logger (Logger, MonadLogger (..))

newtype Env = Env
  { logger :: Logger
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
