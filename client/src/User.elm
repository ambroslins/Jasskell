module User exposing
    ( User
    , decoder
    , name
    )

import Json.Decode as Decode exposing (Decoder)


type User
    = User
        { name : String
        }


name : User -> String
name (User user) =
    user.name


decoder : Decoder User
decoder =
    Decode.field "name" Decode.string
        |> Decode.map (\n -> User { name = n })
