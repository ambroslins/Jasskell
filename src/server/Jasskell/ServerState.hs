{-# LANGUAGE DeriveGeneric #-}
module Jasskell.ServerState where

import           Data.Aeson
import qualified Data.Map                      as Map
import           Data.Map                       ( Map )
import           Data.UUID                      ( UUID )
import           Data.UUID.V4                   ( nextRandom )
import           Control.Monad.STM
import           Control.Concurrent
import           Control.Concurrent.STM.TVar
import           Jasskell.Table
import           Jasskell.User                  ( User )
import           GHC.Generics


newtype TableID = TableID UUID deriving(Eq, Ord, Show, Generic)

instance ToJSON TableID

instance FromJSON TableID

newtype ServerState = ServerState (TVar (Map TableID Table))

emptyServerState :: IO ServerState
emptyServerState = ServerState <$> newTVarIO Map.empty

createGame :: ServerState -> IO TableID
createGame (ServerState var) = do
    tableID <- TableID <$> nextRandom
    table   <- newTable
    atomically $ modifyTVar var (Map.insert tableID table)
    putStrLn $ "Game created: " ++ show tableID
    return tableID

userJoin :: ServerState -> TableID -> User -> IO ()
userJoin (ServerState var) tableID user = do
    table <- Map.lookup tableID <$> readTVarIO var
    case table of
        Just g  -> writeChan (joinChan g) user >> putStrLn "User joined"
        Nothing -> return ()

allGames :: ServerState -> IO [TableID]
allGames (ServerState var) = Map.keys <$> readTVarIO var
