module Jasskell.Server.User
  ( User,
    name,
    sendMessage,
    make,
  )
where

import Control.Concurrent.STM (retry)
import Jasskell.Server.Message (Message)

data User n = User
  { name :: Text,
    sendMessage :: Message n -> STM ()
  }

make :: Text -> STM (User n, STM (Message n))
make username =
  newTVar Nothing <&> \msgVar ->
    let getMsg =
          readTVar msgVar >>= \case
            Nothing -> retry
            Just msg -> msg <$ writeTVar msgVar Nothing
        putMsg msg = writeTVar msgVar (Just msg)
        user =
          User
            { name = username,
              sendMessage = putMsg
            }
     in (user, getMsg)