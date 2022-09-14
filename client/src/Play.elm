module Play exposing (Model, Msg, init, subscriptions, update, view)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Play.Waiting as Waiting
import Route
import Seat exposing (Seat)
import TableID exposing (TableID)
import Vector exposing (Vector)
import Vector.Index exposing (Index(..))
import WebSocket



-- MODEL


type Model
    = Connecting
    | Connected (Maybe State)
    | Error String


type alias State =
    { seats : Vector Seat
    , phase : Phase
    }


type Phase
    = Waiting Waiting.Model


init : TableID -> ( Model, Cmd Msg )
init tableID =
    ( Connecting
    , WebSocket.open <| Route.toString (Route.Play tableID)
    )



-- UPDATE


type Msg
    = WebSocketEvent WebSocket.Event
    | WaitingMsg Waiting.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WebSocketEvent event ->
            case event of
                WebSocket.Open ->
                    ( Connected Nothing, Cmd.none )

                WebSocket.Message message ->
                    case Decode.decodeString messageDecoder message of
                        Err e ->
                            ( Error <| Decode.errorToString e
                            , Cmd.none
                            )

                        Ok state ->
                            ( Connected (Just state)
                            , Cmd.none
                            )

                WebSocket.Error ->
                    ( Error "websocket error", Cmd.none )

                WebSocket.Close ->
                    ( Error "websocket close", Cmd.none )

        WaitingMsg subMsg ->
            case model of
                Connected (Just state) ->
                    case state.phase of
                        Waiting subModel ->
                            let
                                ( newSubModel, cmd ) =
                                    Waiting.update subMsg subModel
                            in
                            ( Connected (Just { state | phase = Waiting newSubModel })
                            , Cmd.map WaitingMsg cmd
                            )

                _ ->
                    ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "w-screen h-screen flex justify-center items-center" ]
        [ case model of
            Connecting ->
                div [ class "text-8xl" ] [ text "Connecting" ]

            Connected Nothing ->
                text ""

            Connected (Just state) ->
                viewState state

            Error s ->
                div [ class "text-4xl text-red-500" ]
                    [ text <| "Error: " ++ s ]
        ]


viewState : State -> Html Msg
viewState state =
    case state.phase of
        Waiting model ->
            Html.map WaitingMsg <| Waiting.view state.seats model



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    WebSocket.subscribe WebSocketEvent



-- DECODERS


stateDecoder : Decoder State
stateDecoder =
    Decode.succeed State
        |> Decode.required "seats" (Vector.decode Seat.decode)
        |> Decode.hardcoded (Waiting Waiting.init)


messageDecoder : Decoder State
messageDecoder =
    Decode.oneOf
        [ Decode.field "guest-view" stateDecoder
        , Decode.field "player-view" stateDecoder
        ]
