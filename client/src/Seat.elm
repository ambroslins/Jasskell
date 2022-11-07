module Seat exposing (..)

import Json.Decode as Decode exposing (Decoder)
import User exposing (User)


type Seat
    = Empty
    | Taken User


fromMaybe : Maybe User -> Seat
fromMaybe maybeUser =
    case maybeUser of
        Nothing ->
            Empty

        Just user ->
            Taken user


decoder : Decoder Seat
decoder =
    Decode.map fromMaybe (Decode.nullable User.decoder)
