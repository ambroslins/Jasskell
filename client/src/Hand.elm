module Hand exposing (Hand, decode, view)

import Card exposing (Card)
import Html exposing (Html, button, div)
import Html.Attributes exposing (class)
import Json.Decode as Decode exposing (Decoder)


type Hand
    = Hand (List Card)


decode : Decoder Hand
decode =
    Decode.map Hand <|
        Decode.list Card.decode


view : Hand -> Html msg
view (Hand cards) =
    let
        viewCard c =
            button
                [ class "w-40 -ml-12 hover:z-10" ]
                [ Card.view c ]
    in
    div [ class "flex flex-row justify-center items-center pl-12 mb-4" ] <|
        List.map viewCard cards
