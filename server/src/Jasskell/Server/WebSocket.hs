module Jasskell.Server.WebSocket where

import Control.Concurrent.Async (concurrently_)
import Data.Aeson.Combinators.Decode (Decoder)
import Data.Aeson.Combinators.Decode qualified as Decoder
import Data.Aeson.Combinators.Encode qualified as Encoder
import Jasskell.Server.Decoder qualified as Decoder
import Jasskell.Server.Encoder qualified as Encoder
import Jasskell.Server.ServerState (ServerState, SomeTable (..))
import Jasskell.Server.ServerState qualified as ServerState
import Jasskell.Server.Table (Connection (..))
import Jasskell.Server.Table qualified as Table
import Jasskell.Server.TableID (TableID)
import Jasskell.Server.User (User (User))
import Network.WebSockets
  ( PendingConnection (pendingRequest),
    ServerApp,
    acceptRequest,
    receiveData,
    sendTextData,
    withPingThread,
  )

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

server :: ServerState -> ServerApp
server serverState pendingConnection = do
  print $ pendingRequest pendingConnection
  connection <- acceptRequest pendingConnection
  withPingThread connection 30 pass $ do
    joinMessage <- receiveData connection
    case Decoder.decode joinMessageDecoder joinMessage of
      Nothing -> putStrLn . decodeUtf8 $ "bad join message: " <> joinMessage
      Just JoinMessage {username, tableID, seat} ->
        atomically (ServerState.lookupTable serverState tableID) >>= \case
          Nothing -> putStrLn "lookup failed"
          Just (SomeTable table) -> do
            Just tableConnection <-
              atomically $
                Table.join (User username) (fromIntegral seat) table

            let sendThread = forever $ do
                  view <- atomically $ getView tableConnection
                  let message = Encoder.encode Encoder.view view
                  sendTextData connection message

            let reciveThread = forever $ do
                  message <- receiveData connection
                  case Decoder.decode Decoder.action message of
                    Nothing -> sendTextData @Text connection "bad action"
                    Just action ->
                      atomically (putAction tableConnection action) >>= \case
                        Left err -> print err
                        Right () -> pure ()

            concurrently_ sendThread reciveThread
