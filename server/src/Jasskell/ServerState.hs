module Jasskell.ServerState where

import           Data.Aeson
import qualified Data.Map                      as Map
import           Data.Map                       ( Map )
import           Data.Text                      ( pack
                                                , unpack
                                                )
import           Data.UUID                      ( UUID )
import           Data.UUID.V4                   ( nextRandom )
import           Control.Monad.STM
import           Control.Concurrent
import           Control.Concurrent.STM.TVar
import           Text.Read                      ( readMaybe )
import           Jasskell.Game
import           Jasskell.User                  ( User )


newtype GameID = GameID UUID deriving (Eq, Ord, Show, Read)

instance ToJSON GameID where
    toJSON = String . pack . show

instance FromJSON GameID where
    parseJSON = withText "gameID" $ \t -> case readMaybe $ unpack t of
        Just g  -> pure g
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
