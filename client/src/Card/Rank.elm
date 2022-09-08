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
            "Six"

        Seven ->
            "Seven"

        Eight ->
            "Eight"

        Nine ->
            "Nine"

        Ten ->
            "Ten"

        Under ->
            "Under"

        Over ->
            "Over"

        King ->
            "King"

        Ace ->
            "Ace"


encode : Rank -> Value
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
                        Decode.fail ("Invalid rank: " ++ r)
            )
