module Jasskell.ServerState where

import qualified Data.Map                      as Map
import           Data.Map                       ( Map )
import           Control.Monad.STM
import           Control.Concurrent
import           Control.Concurrent.STM.TVar
import           Jasskell.Game
import           Jasskell.GameID
import           Jasskell.User                  ( User )



newtype ServerState = ServerState (TVar (Map GameID Game))

emptyServerState :: IO ServerState
emptyServerState = ServerState <$> newTVarIO Map.empty

createGame :: ServerState -> IO GameID
createGame (ServerState var) = do
    gameID <- newGameID
    game   <- newGame
    atomically $ modifyTVar var (Map.insert gameID game)
    putStrLn $ "Game created: " ++ show gameID
    return gameID

userJoin :: ServerState -> GameID -> User -> IO ()
userJoin (ServerState var) gameID user = do
    game <- Map.lookup gameID <$> readTVarIO var
    case game of
        Just g  -> writeChan (joinChan g) user >> putStrLn "User joined"
        Nothing -> return ()

allGames :: ServerState -> IO [GameID]
allGames (ServerState var) = Map.keys <$> readTVarIO var
