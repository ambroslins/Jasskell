module Card.Suit exposing (Suit, decode, encode, toString)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type Suit
    = Bells
    | Hearts
    | Acorns
    | Leaves


toString : Suit -> String
toString s =
    case s of
        Bells ->
            "Bells"

        Hearts ->
            "Hearts"

        Acorns ->
            "Acorns"

        Leaves ->
            "Leaves"


encode : Suit -> Encode.Value
encode =
    Encode.string << toString


decode : Decoder Suit
decode =
    Decode.string
        |> Decode.andThen
            (\s ->
                case s of
                    "Bells" ->
                        Decode.succeed Bells

                    "Hearts" ->
                        Decode.succeed Hearts

                    "Acorns" ->
                        Decode.succeed Acorns

                    "Leaves" ->
                        Decode.succeed Leaves

                    _ ->
                        Decode.fail ("Unkown suit: " ++ s)
            )
