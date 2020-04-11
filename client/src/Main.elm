module Main exposing (..)

import Array exposing (Array)
import Browser
import Card exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
import WebSocket


main : Program () Model Msg
main =
    Browser.document { init = init, update = update, view = view, subscriptions = subscriptions }


encodeCard : Card -> Encode.Value
encodeCard c =
    Encode.object [ ( "suit", encodeSuit c.suit ), ( "rank", encodeRank c.rank ) ]


decodeCard : Decode.Decoder Card
decodeCard =
    Decode.succeed Card
        |> Pipeline.required "suit" decodeSuit
        |> Pipeline.required "rank" decodeRank


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
                    |> Pipeline.required "card" decodeCard
                    |> Pipeline.required "playable" Decode.bool
                )
            )
        |> Pipeline.required "table" (Decode.array (Decode.nullable decodeCard))


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
            , WebSocket.send (Encode.object [ ( "playCard", encodeCard c ) ])
            )

        Update r ->
            case r of
                Ok m ->
                    ( m, Cmd.none )

                Err e ->
                    ( Debug.log (Decode.errorToString e) model, Cmd.none )


viewCard : Card -> Html Msg
viewCard card =
    div
        [ class "card"
        ]
        [ p [] [ text (showCard card) ] ]


viewHandCard : Int -> Int -> ( Card, Bool ) -> ( String, Html Msg )
viewHandCard size i ( card, p ) =
    let
        angle =
            0.1
                * (toFloat i
                    - (toFloat (size - 1) / 2)
                  )

        radius =
            50
    in
    ( showCard card
    , div
        [ onClick
            (if p then
                PlayCard card

             else
                NoOp
            )
        , classList [ ( "card-hand", True ), ( "card-hand-playable", p ) ]
        , style "transform"
            (String.concat
                [ "translate("
                    ++ String.fromFloat (sin angle * radius)
                    ++ "rem, "
                    ++ String.fromFloat ((1.0 - cos angle) * radius)
                    ++ "rem) "
                , "rotate(" ++ String.fromFloat angle ++ "rad)"
                ]
            )
        ]
        [ viewCard card ]
    )


viewHand : Array ( Card, Bool ) -> Html Msg
viewHand hand =
    Keyed.ul
        [ class "hand"
        ]
        (List.indexedMap (viewHandCard (Array.length hand)) (Array.toList hand))


viewNoCard : Html Msg
viewNoCard =
    div
        [ class "card"
        ]
        []


viewTabel : Array (Maybe Card) -> Html Msg
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
            (Array.toList table)
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
    WebSocket.receive
        (\r ->
            case r of
                Err e ->
                    Debug.log (Decode.errorToString e) NoOp

                Ok e ->
                    case e of
                        WebSocket.Message m ->
                            Update (Decode.decodeString decodeModel m)

                        _ ->
                            Debug.log "unkown event" NoOp
        )
