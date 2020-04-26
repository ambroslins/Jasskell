{-# LANGUAGE OverloadedStrings #-}

module Jasskell.Server where

import           Data.Aeson
import qualified Data.Text.Lazy                as Text
import           Data.Text.Lazy                 ( pack )
import           Jasskell.Message
import           Jasskell.ServerState
import           Jasskell.User
import           Network.Wai.Middleware.Static
import           Network.Wai.Handler.WebSockets
import qualified Network.WebSockets            as WS
import           Web.Scotty
import           Control.Concurrent
import           Control.Monad


server :: IO ()
server = do
    state <- emptyServerState
    scotty 9000 $ do
        middleware $ staticPolicy (addBase "../client")
        middleware $ websocketsOr WS.defaultConnectionOptions (wsServer state)
        get "/" $ file "../client/index.html"
        get "/newgame" $ do
            gameID <- liftAndCatchIO $ createGame state
            text $ pack (show gameID)
        get "/games" $ do
            games <- liftAndCatchIO $ allGames state
            text $ Text.unlines $ map (pack . show) games

wsServer :: ServerState -> WS.ServerApp
wsServer state pen = do
    c <- WS.acceptRequest pen
    putStrLn "accepted connection"
    chan <- newChan
    let receive = do
            ma <- decode <$> WS.receiveData c
            maybe (return ()) (writeChan chan) ma
    let u = newUser (readChan chan)
                    (\(UpdateGameView g) -> WS.sendTextData c $ encode g)
                    "Websocket User"
    game <- head <$> allGames state
    userJoin state game u
    forever receive
