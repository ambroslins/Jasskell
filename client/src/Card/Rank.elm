module Card.Rank exposing (Rank(..), decode, encode, toString)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type Rank
    = Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Under
    | Over
    | King
    | Ace


toString : Rank -> String
toString r =
    case r of
        Six ->
            "six"

        Seven ->
            "seven"

        Eight ->
            "eight"

        Nine ->
            "nine"

        Ten ->
            "ten"

        Under ->
            "under"

        Over ->
            "over"

        King ->
            "king"

        Ace ->
            "ace"


encode : Rank -> Value
encode =
    Encode.string << toString


decode : Decoder Rank
decode =
    Decode.string
        |> Decode.andThen
            (\r ->
                case r of
                    "six" ->
                        Decode.succeed Six

                    "seven" ->
                        Decode.succeed Seven

                    "eight" ->
                        Decode.succeed Eight

                    "nine" ->
                        Decode.succeed Nine

                    "ten" ->
                        Decode.succeed Ten

                    "under" ->
                        Decode.succeed Under

                    "over" ->
                        Decode.succeed Over

                    "king" ->
                        Decode.succeed King

                    "ace" ->
                        Decode.succeed Ace

                    _ ->
                        Decode.fail ("Invalid rank: " ++ r)
            )
