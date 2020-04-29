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
import Page.Home as Home
import WebSocket



-- MODEL


type Model
    = HomePage Home.Model
    | Game GameState


type alias GameState =
    { hand : Array ( Card, Bool )
    , table : Array (Maybe Card)
    }


decodeGameState : Decoder GameState
decodeGameState =
    Decode.succeed GameState
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
    ( HomePage Home.init, Cmd.none )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Jasskell"
    , body =
        case model of
            HomePage home ->
                [ Html.map HomeMsg (Home.view home) ]

            Game state ->
                [ viewTabel state.table, viewHand state.hand ]
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
        , class "card-hand"
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
    | Update (Result Decode.Error GameState)
    | HomeMsg Home.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        PlayCard c ->
            ( model
            , WebSocket.send (Encode.object [ ( "playCard", Card.encode c ) ])
            )

        Update r ->
            case r of
                Ok m ->
                    ( Game m, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        HomeMsg subMsg ->
            case model of
                HomePage home ->
                    let
                        ( m, c ) =
                            Home.update subMsg home
                    in
                    ( HomePage m, Cmd.map HomeMsg c )

                _ ->
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
                            Update (Decode.decodeString decodeGameState m)

                        WebSocket.Open ->
                            HomeMsg Home.OnOpen

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
