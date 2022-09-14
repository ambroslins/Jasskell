module Seat exposing (Seat(..), User, decode)

import Json.Decode as Decode exposing (Decoder)


type Seat
    = Empty
    | Taken User


type alias User =
    { name : String
    }


decode : Decoder Seat
decode =
    Decode.oneOf
        [ Decode.field "empty" (Decode.succeed Empty)
        , Decode.field "taken"
            (Decode.map (Taken << User) <|
                Decode.field "name" Decode.string
            )
        ]
