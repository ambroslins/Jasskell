module Card exposing (Card, decode, encode, toString, view)

import Card.Rank as Rank exposing (Rank)
import Card.Suit as Suit exposing (Suit)
import Html exposing (Attribute, Html, div, p, text)
import Html.Attributes exposing (class)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode


type alias Card =
    { suit : Suit
    , rank : Rank
    }


toString : Card -> String
toString c =
    String.left 1 (Suit.toString c.suit)
        ++ Rank.toString c.rank


encode : Card -> Encode.Value
encode c =
    Encode.object
        [ ( "suit", Suit.encode c.suit )
        , ( "rank", Rank.encode c.rank )
        ]


decode : Decoder Card
decode =
    Decode.succeed Card
        |> required "suit" Suit.decode
        |> required "rank" Rank.decode


view : List (Attribute msg) -> Card -> Html msg
view a card =
    div (a ++ [ class "card" ]) [ p [] [ text (toString card) ] ]
