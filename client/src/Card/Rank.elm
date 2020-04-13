module Card.Rank exposing (Rank(..), decode, encode, toString)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


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
            "6"

        Seven ->
            "7"

        Eight ->
            "8"

        Nine ->
            "9"

        Ten ->
            "X"

        Under ->
            "U"

        Over ->
            "O"

        King ->
            "K"

        Ace ->
            "A"


encode : Rank -> Encode.Value
encode =
    Encode.string << toString


decode : Decoder Rank
decode =
    Decode.string
        |> Decode.andThen
            (\r ->
                case r of
                    "Six" ->
                        Decode.succeed Six

                    "Seven" ->
                        Decode.succeed Seven

                    "Eight" ->
                        Decode.succeed Eight

                    "Nine" ->
                        Decode.succeed Nine

                    "Ten" ->
                        Decode.succeed Ten

                    "Under" ->
                        Decode.succeed Under

                    "Over" ->
                        Decode.succeed Over

                    "King" ->
                        Decode.succeed King

                    "Ace" ->
                        Decode.succeed Ace

                    _ ->
                        Decode.fail ("Unkown rank: " ++ r)
            )
