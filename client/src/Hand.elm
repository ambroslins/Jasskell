module Hand exposing (Hand, decode, view)

import Card exposing (Card)
import Html exposing (Html, button, li, ul)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Json.Decode as Decode exposing (Decoder)


type Hand
    = Hand (List Card)


decode : Decoder Hand
decode =
    Decode.map Hand <|
        Decode.list Card.decode


view : (Card -> msg) -> Hand -> Html msg
view playCard (Hand cards) =
    let
        viewCard c =
            li [ class "w-40 -ml-12 first:ml-0" ]
                [ button
                    [ class "w-full hover:z-10"
                    , onClick (playCard c)
                    ]
                    [ Card.view c ]
                ]
    in
    ul [ class "flex flex-row justify-center items-center mb-4 list-none" ]
        (List.map viewCard cards)
