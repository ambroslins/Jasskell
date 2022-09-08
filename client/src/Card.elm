module Card exposing (Card, decode, encode)

import Card.Rank as Rank exposing (Rank)
import Card.Suit as Suit exposing (Suit)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type alias Card =
    { suit : Suit
    , rank : Rank
    }


encode : Card -> Value
encode card =
    Encode.object
        [ ( "suit", Suit.encode card.suit )
        , ( "rank", Rank.encode card.rank )
        ]


decode : Decoder Card
decode =
    Decode.map2 Card
        (Decode.field "suit" Suit.decode)
        (Decode.field "rank" Rank.decode)
