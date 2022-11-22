module Variant exposing (..)

import Card.Suit as Suit exposing (Suit)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type Variant
    = Trump Suit
    | Direction Direction
    | Slalom Direction


type Direction
    = TopDown
    | BottomUp


encode : Variant -> Value
encode variant =
    case variant of
        Trump suit ->
            Encode.object [ ( "trump", Suit.encode suit ) ]

        Direction direction ->
            Encode.object [ ( "direction", encodeDirection direction ) ]

        Slalom direction ->
            Encode.object [ ( "slalom", encodeDirection direction ) ]


encodeDirection : Direction -> Value
encodeDirection direction =
    case direction of
        TopDown ->
            Encode.string "topdown"

        BottomUp ->
            Encode.string "bottomup"


decode : Decoder Variant
decode =
    Decode.oneOf
        [ Decode.field "trump" (Decode.map Trump Suit.decode)
        , Decode.field "direction" (Decode.map Direction decodeDirection)
        , Decode.field "slalom" (Decode.map Slalom decodeDirection)
        ]


decodeDirection : Decoder Direction
decodeDirection =
    Decode.string
        |> Decode.andThen
            (\direction ->
                case direction of
                    "topdown" ->
                        Decode.succeed TopDown

                    "bottomup" ->
                        Decode.succeed BottomUp

                    _ ->
                        Decode.fail ("Invalid direction: " ++ direction)
            )
