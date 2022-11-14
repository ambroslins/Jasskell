module Jasskell.Server.WebSocket where

import Colog.Message (logError, logInfo, logWarning)
import Data.Aeson.Combinators.Decode (Decoder)
import Data.Aeson.Combinators.Decode qualified as Decoder
import Data.Aeson.Combinators.Encode qualified as Encoder
import Data.ByteString qualified as ByteString
import Data.Text qualified as Text
import Jasskell.Server.App (MonadApp)
import Jasskell.Server.App qualified as App
import Jasskell.Server.Decoder qualified as Decoder
import Jasskell.Server.Message qualified as Message
import Jasskell.Server.Table (SomeTable (SomeTable))
import Jasskell.Server.Table qualified as Table
import Jasskell.Server.TableID (TableID)
import Jasskell.Server.TableID qualified as TableID
import Network.WebSockets
  ( PendingConnection (pendingRequest),
    ServerApp,
    acceptRequest,
    receiveData,
    rejectRequest,
    requestPath,
    sendTextData,
    withPingThread,
  )
import UnliftIO (MonadUnliftIO, toIO, withException, withRunInIO)

data JoinMessage = JoinMessage
  { username :: Text,
    tableID :: TableID,
    seat :: Int
  }
  deriving (Eq, Show)

joinMessageDecoder :: Decoder JoinMessage
joinMessageDecoder =
  JoinMessage
    <$> Decoder.key "username" Decoder.text
    <*> Decoder.key "tableID" Decoder.auto
    <*> Decoder.key "seat" Decoder.int

server :: (MonadApp m, MonadUnliftIO m) => m ServerApp
server = withRunInIO $ \runInIO -> pure (runInIO . handleConnection)

handleConnection :: (MonadApp m, MonadUnliftIO m) => PendingConnection -> m ()
handleConnection pendingConnection = do
  let path = requestPath $ pendingRequest pendingConnection
  case ByteString.stripPrefix "/play/" path >>= TableID.fromByteString of
    Nothing -> do
      liftIO $ rejectRequest pendingConnection $ "Invalid path: " <> path
      logInfo $
        "WebSocket connection rejected because the path: "
          <> show path
          <> " is invalid"
    Just tableID ->
      App.lookupTable tableID >>= \case
        Nothing -> do
          liftIO $ rejectRequest pendingConnection "Table not found"
          logInfo $
            "WebSocket connection rejected becuase the table: "
              <> show tableID
              <> " does not exist"
        Just (SomeTable table) -> do
          connection <- liftIO $ acceptRequest pendingConnection
          logInfo "WebSocket connection accepted"

          let sendMessage =
                sendTextData connection
                  . Encoder.encode Message.encoder
              reciveAction =
                Decoder.eitherDecode Decoder.action
                  <$> receiveData connection

          sendAction <- Table.join sendMessage table

          run <-
            toIO . forever $
              liftIO reciveAction >>= \case
                Left e ->
                  logWarning ("Failed to decode client action: " <> Text.pack e)
                Right action -> liftIO (sendAction action)

          liftIO (withPingThread connection 30 pass run)
            `withException` \(SomeException e) ->
              logError ("WebSocket connection ended with: " <> show e)
