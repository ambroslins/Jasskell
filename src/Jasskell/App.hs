module Jasskell.App
  ( Env (..),
    AppT,
    runAppT,
  )
where

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
import Jasskell.Logger (Logger, MonadLogger (..))

newtype Env = Env {logger :: Logger}

newtype AppT m a = AppT (ReaderT Env m a)
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadUnliftIO,
      MonadReader Env
    )

instance (MonadIO m) => MonadRandom (AppT m) where
  getRandomBytes = liftIO . getRandomBytes

instance (Monad m) => MonadLogger (AppT m) where
  askLogger = asks (.logger)

instance MonadTrans AppT where
  lift = AppT . lift

runAppT :: Env -> AppT m a -> m a
runAppT env (AppT m) = runReaderT m env
