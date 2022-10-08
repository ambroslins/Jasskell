module Jasskell.Server.Player
  ( Player,
    make,
    user,
    client,
  )
where

import Jasskell.Server.Client (Client)
import Jasskell.Server.User (User)

data Player n = MakePlayer
  { user :: User,
    client :: Client n
  }

make :: User -> Client n -> Player n
make = MakePlayer
