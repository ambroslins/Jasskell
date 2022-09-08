module Card exposing (Card, decode, encode, view)

import Card.Rank as Rank exposing (Rank)
import Card.Suit as Suit exposing (Suit)
import Html exposing (Html, p, text)
import Html.Attributes exposing (class)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Html exposing (div)


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


view : Card -> Html msg
view card =
    div
        [ class "aspect-card rounded-3xl text-center border-4 border-black bg-gray-100" ]
        [ p [] [ text <| Suit.toString card.suit ]
        , p [] [ text <| Rank.toString card.rank ]
        ]
