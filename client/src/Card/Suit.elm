module Card.Suit exposing (Suit(..), decode, encode, toString)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type Suit
    = Bells
    | Hearts
    | Acorns
    | Leaves


toString : Suit -> String
toString s =
    case s of
        Bells ->
            "bells"

        Hearts ->
            "hearts"

        Acorns ->
            "acorns"

        Leaves ->
            "leaves"


encode : Suit -> Value
encode =
    Encode.string << toString


decode : Decoder Suit
decode =
    Decode.string
        |> Decode.andThen
            (\s ->
                case s of
                    "bells" ->
                        Decode.succeed Bells

                    "hearts" ->
                        Decode.succeed Hearts

                    "acorns" ->
                        Decode.succeed Acorns

                    "leaves" ->
                        Decode.succeed Leaves

                    _ ->
                        Decode.fail ("Invalid suit: " ++ s)
            )
