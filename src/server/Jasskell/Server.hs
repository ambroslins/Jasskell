{-# LANGUAGE OverloadedStrings #-}

module Jasskell.Server where

import           Jasskell.ServerState
import qualified Jasskell.Server.WebSocket     as WebSocketServer
import           Network.Wai.Middleware.Static
import           Web.Scotty


server :: IO ()
server = do
    state <- emptyServerState
    scotty 9000 $ do
        middleware $ staticPolicy (addBase "static")
        middleware $ WebSocketServer.middleware state
        post "/newgame" $ do
            gameID <- liftAndCatchIO $ createGame state
            json gameID
        get "/games" $ do
            games <- liftAndCatchIO $ allGames state
            json games
        notFound $ file "static/index.html"
