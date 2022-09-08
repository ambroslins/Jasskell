module Main exposing (..)

import Browser
import Card exposing (Card)
import Card.Rank as Rank
import Card.Suit as Suit
import Html exposing (Html, button, div)
import Html.Attributes exposing (class)



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }



-- MODEL


type alias Model =
    { hand : List Card
    , table : Table
    }


type alias Table =
    { self : Maybe Card
    , left : Maybe Card
    , opposite : Maybe Card
    , right : Maybe Card
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { hand =
            [ { suit = Suit.Bells, rank = Rank.Six }
            , { suit = Suit.Bells, rank = Rank.Under }
            , { suit = Suit.Hearts, rank = Rank.Ten }
            , { suit = Suit.Acorns, rank = Rank.King }
            , { suit = Suit.Leaves, rank = Rank.Nine }
            ]
      , table =
            { self = Nothing
            , left = Just { suit = Suit.Acorns, rank = Rank.Eight }
            , opposite = Nothing
            , right = Nothing
            }
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "w-screen h-screen flex justify-center items-center" ]
        [ div [ class "max-w-screen-xl h-screen flex flex-col items-center justify-end" ]
            [ viewTable model.table
            , viewHand model.hand
            ]
        ]


viewTable : Table -> Html Msg
viewTable table =
    let
        placeholder =
            div [ class "aspect-card border-2 rounded-3xl bg-gray-100" ]
                []

        viewSeat mc =
            div [ class "w-32" ]
                [ case mc of
                    Nothing ->
                        placeholder

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


viewHand : List Card -> Html Msg
viewHand =
    let
        viewCard c =
            button
                [ class "w-40 -ml-12 hover:z-10" ]
                [ Card.view c ]
    in
    div [ class "flex flex-row justify-center items-center pl-12 mb-4" ]
        << List.map viewCard
