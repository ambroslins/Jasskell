module Jasskell.User
    ( User
    , name
    , getAction
    , putMessage
    , cliUser
    , newUser
    )
where

import           Jasskell.Action
import           Jasskell.Message

data User = User { name :: String
                 , getAction :: IO Action
                 , putMessage :: Message -> IO ()
                 }

newUser :: IO Action -> (Message -> IO ()) -> String -> User
newUser get put n = User { name = n, getAction = get, putMessage = put }

cliUser :: String -> User
cliUser =
    newUser (PlayCard . read <$> getLine) (\(UpdateGameView gv) -> print gv)

