module Jasskell.Database
  ( Config (..),
    Pool,
    withPool,
    use,
  )
where

import Control.Exception (bracket, throwIO)
import Data.Text (Text)
import Data.Word (Word16)
import Hasql.Connection.Settings qualified as Hasql
import Hasql.Pool qualified
import Hasql.Pool.Config qualified
import Hasql.Session qualified as Hasql
import Pqi.Ffi qualified

data Config = Config
  { host :: Text,
    port :: Word16,
    user :: Text,
    password :: Text,
    name :: Text,
    poolSize :: Int
  }
  deriving (Show)

newtype Pool = Pool Hasql.Pool.Pool

withPool :: Config -> (Pool -> IO a) -> IO a
withPool config run =
  bracket
    (Hasql.Pool.acquire Pqi.Ffi.adapter poolConfig)
    Hasql.Pool.release
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

use :: Pool -> Hasql.Session a -> IO a
use (Pool pool) session =
  Hasql.Pool.use pool session >>= either throwIO pure
