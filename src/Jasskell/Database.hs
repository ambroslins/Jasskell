module Jasskell.Database (Config (..), Pool, withPool, use) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Text (Text)
import Data.Word (Word16)
import Hasql.Connection.Settings qualified as Hasql
import Hasql.Pool qualified
import Hasql.Pool.Config qualified
import Hasql.Session qualified as Hasql
import UnliftIO (bracket)

data Config = Config
  { host :: !Text,
    port :: !Word16,
    user :: !Text,
    password :: !Text,
    name :: !Text,
    poolSize :: !Int
  }
  deriving (Show)

newtype Pool = Pool Hasql.Pool.Pool

withPool :: (MonadUnliftIO m) => Config -> (Pool -> m a) -> m a
withPool config run =
  bracket
    (liftIO $ Hasql.Pool.acquire poolConfig)
    (liftIO . Hasql.Pool.release)
    (run . Pool)
  where
    poolConfig =
      Hasql.Pool.Config.settings
        [ Hasql.Pool.Config.size config.poolSize,
          Hasql.Pool.Config.staticConnectionSettings connectionSettings
        ]
    connectionSettings =
      Hasql.hostAndPort config.host config.port
        <> Hasql.user config.user
        <> Hasql.password config.password
        <> Hasql.dbname config.name

use :: (MonadIO m) => (Hasql.Pool.UsageError -> m a) -> Pool -> Hasql.Session a -> m a
use onError (Pool pool) session =
  liftIO (Hasql.Pool.use pool session) >>= either onError pure
