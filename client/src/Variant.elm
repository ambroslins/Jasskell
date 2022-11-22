module Variant exposing
    ( Direction(..)
    , Variant(..)
    , all
    , decode
    , encode
    , toString
    )

import Card.Suit as Suit exposing (Suit(..))
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type Variant
    = Trump Suit
    | Direction Direction
    | Slalom Direction


type Direction
    = TopDown
    | BottomUp


all : List Variant
all =
    [ Trump Bells
    , Trump Hearts
    , Trump Acorns
    , Trump Leaves
    , Direction TopDown
    , Direction BottomUp
    , Slalom TopDown
    , Slalom BottomUp
    ]


toString : Variant -> String
toString variant =
    case variant of
        Trump suit ->
            Suit.toString suit

        Direction direction ->
            directionToString direction

        Slalom direction ->
            "Slalom " ++ directionToString direction


encode : Variant -> Value
encode variant =
    case variant of
        Trump suit ->
            Encode.object [ ( "trump", Suit.encode suit ) ]

        Direction direction ->
            Encode.object [ ( "direction", encodeDirection direction ) ]

        Slalom direction ->
            Encode.object [ ( "slalom", encodeDirection direction ) ]


directionToString : Direction -> String
directionToString direction =
    case direction of
        TopDown ->
            "topdown"

        BottomUp ->
            "bottomup"


encodeDirection : Direction -> Value
encodeDirection =
    Encode.string << directionToString


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
