module Jasskell.Logger
  ( Logger,
    withStderrLogger,
    MonadLogger (..),
    Level (..),
    Pair,
    (=:),
    logDebug,
    logInfo,
    logWarning,
    logError,
    requestLogger,
  )
where

import Control.Concurrent (ThreadId, myThreadId)
import Control.Exception
  ( ExceptionWithContext (..),
    SomeException (..),
    bracket,
    catchNoPropagate,
    rethrowIO,
  )
import Control.Exception.Annotation
  ( SomeExceptionAnnotation (..),
    displayExceptionAnnotation,
  )
import Control.Exception.Context (getAllExceptionAnnotations)
import Control.Monad (when)
import Control.Monad.Except (ExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, ask)
import Control.Monad.Trans (lift)
import Data.Aeson (ToJSON (toEncoding), fromEncoding)
import Data.ByteString.Builder (Builder)
import Data.ByteString.Char8 qualified as BS
import Data.Char qualified as Char
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8Lenient)
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import GHC.Clock (getMonotonicTimeNSec)
import Network.HTTP.Types (Status (..), hContentLength)
import Network.Wai qualified as Wai
import System.Log.FastLogger
  ( LogStr,
    LogType' (..),
    defaultBufSize,
    newFastLogger1,
    toLogStr,
  )
import Prelude hiding (log)

newtype Logger = Logger (Level -> Text -> [Pair] -> IO ())

data Level = Debug | Info | Warning | Error
  deriving (Eq, Show, Ord)

data Pair = Pair {key :: !Text, value :: !Builder}
  deriving (Show)

infix 6 =:

(=:) :: (ToJSON a) => Text -> a -> Pair
key =: value = Pair key (fromEncoding $ toEncoding value)

class MonadLogger m where
  askLogger :: m Logger

instance MonadLogger ((->) Logger) where
  askLogger = id

instance (Monad m) => MonadLogger (ReaderT Logger m) where
  askLogger = ask

instance (Monad m, MonadLogger m) => MonadLogger (ExceptT e m) where
  askLogger = lift askLogger

log :: (MonadIO m, MonadLogger m) => Level -> Text -> [Pair] -> m ()
log level msg pairs = do
  (Logger logger) <- askLogger
  liftIO $ logger level msg pairs

logDebug :: (MonadIO m, MonadLogger m) => Text -> [Pair] -> m ()
logDebug = log Debug

logInfo :: (MonadIO m, MonadLogger m) => Text -> [Pair] -> m ()
logInfo = log Info

logWarning :: (MonadIO m, MonadLogger m) => Text -> [Pair] -> m ()
logWarning = log Warning

logError :: (MonadIO m, MonadLogger m) => Text -> [Pair] -> m ()
logError = log Error

showLevel :: Level -> LogStr
showLevel = \case
  Debug -> "DEBUG"
  Info -> "INFO"
  Warning -> "WARNING"
  Error -> "ERROR"

fmtMessage :: UTCTime -> Level -> ThreadId -> Text -> [Pair] -> LogStr
fmtMessage time level threadId msg pairs =
  "time="
    <> toLogStr (iso8601Show time)
    <> " level="
    <> showLevel level
    <> " thread_id="
    <> toLogStr (dropWhile (not . Char.isDigit) $ show threadId)
    <> " msg="
    <> toLogStr (fromEncoding $ toEncoding msg)
    <> foldMap fmtPair pairs
    <> "\n"
  where
    fmtPair Pair {key, value} = " " <> toLogStr key <> "=" <> toLogStr value

withStderrLogger :: Level -> (Logger -> IO a) -> IO a
withStderrLogger minLevel action =
  bracket (newFastLogger1 $ LogStderr defaultBufSize) snd $
    \(logger, _cleanup) ->
      action $ Logger $ \level msg pairs -> when (level >= minLevel) $ do
        time <- getCurrentTime
        threadId <- myThreadId
        logger $ fmtMessage time level threadId msg pairs

requestLogger :: Logger -> Wai.Middleware
requestLogger (Logger logger) app req respond = do
  start <- getMonotonicTimeNSec
  let logResponse result = do
        end <- getMonotonicTimeNSec
        let !dt = fromIntegral (end - start) * 1e-9 :: Double
            (!status, !size, rest) = case result of
              Left (ExceptionWithContext ctx (SomeException e)) ->
                (500, Nothing, ["exception" =: show e, "context" =: showContext ctx])
              Right response ->
                let (Status st _) = Wai.responseStatus response
                    sz = do
                      h <- lookup hContentLength $ Wai.responseHeaders response
                      (!s, _) <- BS.readInt h
                      pure s
                 in (st, sz, [])
            !level
              | status >= 500 = Error
              | status >= 400 = Warning
              | otherwise = Info
        logger level "request" $
          "method" =: decodeUtf8Lenient (Wai.requestMethod req)
            : "path" =: decodeUtf8Lenient (Wai.rawPathInfo req)
            : "query" =: decodeUtf8Lenient (Wai.rawQueryString req)
            : "status" =: status
            : "size" =: size
            : "duration" =: dt
            : rest

      respondWithLog response = do
        received <- respond response
        logResponse $ Right response
        pure received

  app req respondWithLog
    `catchNoPropagate` (\e -> logResponse (Left e) >> rethrowIO e)
  where
    showContext =
      map (\(SomeExceptionAnnotation e) -> displayExceptionAnnotation e)
        . getAllExceptionAnnotations
