module Jasskell.ServerState where

import           Data.Aeson
import qualified Data.Map                      as Map
import           Data.Map                       ( Map )

import           Data.UUID                      ( UUID
                                                , toString
                                                , toText
                                                , fromText
                                                )
import           Data.UUID.V4                   ( nextRandom )
import           Control.Monad.STM
import           Control.Concurrent
import           Control.Concurrent.STM.TVar
import           Jasskell.Game
import           Jasskell.User                  ( User )


newtype GameID = GameID UUID deriving (Eq, Ord)

instance Show GameID where
    show (GameID uuid) = toString uuid

instance ToJSON GameID where
    toJSON (GameID uuid) = String $ toText uuid

instance FromJSON GameID where
    parseJSON = withText "gameID" $ \t -> case fromText t of
        Just g  -> pure $ GameID g
        Nothing -> fail "not a GameID"

newtype ServerState = ServerState (TVar (Map GameID Game))

emptyServerState :: IO ServerState
emptyServerState = ServerState <$> newTVarIO Map.empty

createGame :: ServerState -> IO GameID
createGame (ServerState var) = do
    gameID <- GameID <$> nextRandom
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
