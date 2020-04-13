module Main exposing (main)

import Array exposing (Array)
import Browser
import Card exposing (Card)
import Card.Rank exposing (Rank(..))
import Card.Suit exposing (Suit(..))
import Html exposing (Html, div, li)
import Html.Attributes exposing (class, classList, style)
import Html.Events exposing (onClick)
import Html.Keyed as Keyed
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
import WebSocket



-- MODEL


type alias Model =
    { hand : Array ( Card, Bool )
    , table : Array (Maybe Card)
    }


decodeModel : Decoder Model
decodeModel =
    Decode.succeed Model
        |> Pipeline.required "hand"
            (Decode.array
                (Decode.succeed Tuple.pair
                    |> Pipeline.required "card" Card.decode
                    |> Pipeline.required "playable" Decode.bool
                )
            )
        |> Pipeline.required "table" (Decode.array (Decode.nullable Card.decode))


init : () -> ( Model, Cmd Msg )
init _ =
    ( { hand =
            Array.fromList
                (List.map
                    (\r -> ( { suit = Bells, rank = r }, True ))
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
                )
      , table = Array.repeat 4 Nothing
      }
    , WebSocket.connect "ws://127.0.0.1:9000"
    )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Jasskell"
    , body =
        [ viewTabel model.table, viewHand model.hand ]
    }


viewHand : Array ( Card, Bool ) -> Html Msg
viewHand hand =
    Keyed.ul
        [ class "hand"
        ]
        (List.indexedMap
            (\i ->
                let
                    offset =
                        toFloat i - (toFloat (Array.length hand - 1) / 2)
                in
                viewHandCard offset
            )
            (Array.toList hand)
        )


viewHandCard : Float -> ( Card, Bool ) -> ( String, Html Msg )
viewHandCard offset ( card, p ) =
    let
        angle =
            0.15
                * offset

        radius =
            50

        atts =
            [ onClick
                (if p then
                    PlayCard card

                 else
                    NoOp
                )
            , classList [ ( "card-playable", p ) ]
            ]
    in
    ( Card.toString card
    , li
        [ style "position" "absolute"
        , style "transform"
            ("translate("
                ++ String.fromFloat (sin angle * radius)
                ++ "rem, "
                ++ String.fromFloat ((1.0 - cos angle) * radius)
                ++ "rem) "
                ++ "rotate("
                ++ String.fromFloat angle
                ++ "rad)"
            )
        ]
        [ Card.view atts card ]
    )


viewNoCard : Html Msg
viewNoCard =
    div
        [ class "card"
        ]
        []


viewTabel : Array (Maybe Card) -> Html Msg
viewTabel table =
    let
        n =
            Array.length table
    in
    div
        [ class "table"
        ]
        (List.indexedMap
            (\i x ->
                let
                    pos =
                        2 * pi * toFloat i / toFloat n
                in
                div
                    [ style "position" "absolute"
                    , style "transform"
                        ("translate("
                            ++ String.fromFloat (-9 * sin pos)
                            ++ "rem, "
                            ++ String.fromFloat (8 * cos pos + 16)
                            ++ "rem)"
                        )
                    ]
                    [ case x of
                        Nothing ->
                            viewNoCard

                        Just c ->
                            Card.view [] c
                    ]
            )
            (Array.toList table)
        )



-- UPDATE


type Msg
    = NoOp
    | PlayCard Card
    | Update (Result Decode.Error Model)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        PlayCard c ->
            ( { model
                | hand = Array.filter (\x -> Tuple.first x /= c) model.hand
              }
            , WebSocket.send (Encode.object [ ( "playCard", Card.encode c ) ])
            )

        Update r ->
            case r of
                Ok m ->
                    ( m, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    WebSocket.receive
        (\r ->
            case r of
                Err _ ->
                    NoOp

                Ok e ->
                    case e of
                        WebSocket.Message m ->
                            Update (Decode.decodeString decodeModel m)

                        _ ->
                            NoOp
        )



-- MAIN


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
