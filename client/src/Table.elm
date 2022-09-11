module Table exposing (Table, decode, view)

import Card exposing (Card)
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Json.Decode as Decode exposing (Decoder)


type Table
    = Table
        { self : Maybe Card
        , left : Maybe Card
        , opposite : Maybe Card
        , right : Maybe Card
        }


decode : Decoder Table
decode =
    Decode.list (Decode.maybe Card.decode)
        |> Decode.andThen
            (\cs ->
                case cs of
                    [ self, left, opposite, right ] ->
                        Decode.succeed <|
                            Table
                                { self = self
                                , left = left
                                , opposite = opposite
                                , right = right
                                }

                    _ ->
                        Decode.fail "Invalid number of table cards"
            )


view : Table -> Html msg
view (Table table) =
    let
        viewSeat mc =
            div [ class "w-32" ]
                [ case mc of
                    Nothing ->
                        Card.placeholder

                    Just c ->
                        Card.view c
                ]
    in
    div [ class "flex flex-col justify-center items-center pb-8" ]
        [ viewSeat table.opposite
        , div [ class "flex flex-row, -mt-16 -mb-16 space-x-16" ]
            [ viewSeat table.left, viewSeat table.right ]
        , viewSeat table.self
        ]
