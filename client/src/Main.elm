module Main exposing (..)

import Browser
import Card exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Encode as Encode
import WebSocket


main : Program () Model Msg
main =
    Browser.document { init = init, update = update, view = view, subscriptions = subscriptions }


type alias Model =
    { hand : List Card
    , table : List (Maybe Card)
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { hand =
            List.map (\r -> { suit = Bells, rank = r })
                [ Six
                , Seven
                , Eight
                , Nine
                , Ten
                , Under
                , Over
                , King
                , Ace
                ]
      , table = List.repeat 4 Nothing
      }
    , WebSocket.connect "ws://127.0.0.1:9000"
    )


type Msg
    = PlayCard Card


addCardToTable : Card -> List (Maybe Card) -> List (Maybe Card)
addCardToTable c cs =
    case cs of
        [] ->
            []

        Nothing :: xs ->
            Just c :: xs

        x :: xs ->
            x :: addCardToTable c xs


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PlayCard c ->
            ( { model
                | hand = List.filter (\x -> x /= c) model.hand
                , table = addCardToTable c model.table
              }
            , WebSocket.send (Encode.string (showCard c))
            )


viewCard : Card -> Html Msg
viewCard card =
    div
        [ class "card"
        ]
        [ text (showCard card) ]


viewHandCard : Int -> Int -> Card -> Html Msg
viewHandCard size i card =
    let
        angle =
            0.1
                * (toFloat i
                    - (toFloat (size - 1) / 2)
                  )
    in
    div
        [ onClick (PlayCard card)
        , style "translateX" (String.fromFloat (sin angle * 100))
        , style "translateY" (String.fromFloat ((1.0 - cos angle) * 100))
        , style "rotate" (String.fromFloat angle)
        , style "positon" "absolute"
        ]
        [ viewCard card ]


viewHand : List Card -> Html Msg
viewHand hand =
    div
        [ class "hand"
        ]
        (List.indexedMap (viewHandCard (List.length hand)) hand)


viewNoCard : Html Msg
viewNoCard =
    div
        [ class "card"
        , style "border-color" "black"
        ]
        []


viewTabel : List (Maybe Card) -> Html Msg
viewTabel table =
    div
        [ class "table"
        ]
        (List.map
            (\x ->
                case x of
                    Nothing ->
                        viewNoCard

                    Just c ->
                        viewCard c
            )
            table
        )


view : Model -> Browser.Document Msg
view model =
    { title = "Jasskell"
    , body =
        [ div [] [ viewTabel model.table, viewHand model.hand ]
        ]
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
