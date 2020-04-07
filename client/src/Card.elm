module Card exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (..)
import Json.Encode as Encode


type Suit
    = Bells
    | Hearts
    | Acorns
    | Leaves


showSuit : Suit -> String
showSuit s =
    case s of
        Bells ->
            "Bells"

        Hearts ->
            "Hearts"

        Acorns ->
            "Acorns"

        Leaves ->
            "Leaves"


encodeSuit : Suit -> Encode.Value
encodeSuit =
    Encode.string << showSuit


decodeSuit : Decoder Suit
decodeSuit =
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


showRank : Rank -> String
showRank r =
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


encodeRank : Rank -> Encode.Value
encodeRank =
    Encode.string << showRank


decodeRank : Decoder Rank
decodeRank =
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


type alias Card =
    { suit : Suit
    , rank : Rank
    }


showCard : Card -> String
showCard c =
    showSuit c.suit ++ " " ++ showRank c.rank


encodeCard : Card -> Encode.Value
encodeCard c =
    Encode.object
        [ ( "suit", encodeSuit c.suit )
        , ( "rank", encodeRank c.rank )
        ]


decodeCard : Decoder Card
decodeCard =
    Decode.succeed Card
        |> required "suit" decodeSuit
        |> required "rank" decodeRank
