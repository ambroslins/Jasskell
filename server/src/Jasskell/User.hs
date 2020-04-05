module Jasskell.User
    ( User
    , name
    , getAction
    , putMessage
    , cliUser
    , websocketUser
    )
where

import           Data.Aeson
import           Network.WebSockets             ( Connection
                                                , receiveData
                                                , sendTextData
                                                )
import           Jasskell.Action
import           Jasskell.Message

data User = User { name :: String
                 , getAction :: IO Action
                 , putMessage :: Message -> IO ()
                 }

cliUser :: String -> User
cliUser n = User { name       = n
                 , getAction  = PlayCard . read <$> getLine
                 , putMessage = \(UpdateGameView gv) -> print gv
                 }

websocketUser :: Connection -> String -> User
websocketUser c n = User
    { name       = n
    , getAction  = get
    , putMessage = \(UpdateGameView g) -> sendTextData c $ encode g
    }
    where get = decode <$> receiveData c >>= \a -> maybe get return a
