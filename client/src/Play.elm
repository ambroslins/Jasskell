module Play exposing (main)

import Browser
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Pipeline as Decode
import Play.Waiting as Waiting
import TableID
import WebSocket


main : Program Value Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type Model
    = Connecting
    | Waiting Waiting.Model
    | Error String


init : Value -> ( Model, Cmd Msg )
init flags =
    case Decode.decodeValue TableID.decode flags of
        Ok tableID ->
            ( Connecting, WebSocket.open ("/play/" ++ TableID.toString tableID) )

        Err error ->
            ( Error (Decode.errorToString error), Cmd.none )



-- UPDATE


type Msg
    = WebSocketEvent WebSocket.Event
    | WaitingMsg Waiting.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( WebSocketEvent event, _ ) ->
            case event of
                WebSocket.Open ->
                    ( model, Cmd.none )

                WebSocket.Message message ->
                    case Decode.decodeString messageDecoder message of
                        Ok updateModel ->
                            ( updateModel model, Cmd.none )

                        Err error ->
                            ( Error (Decode.errorToString error), Cmd.none )

                WebSocket.Error ->
                    ( Error "websocket error", Cmd.none )

                WebSocket.Close ->
                    ( Error "websocket close", Cmd.none )

        ( WaitingMsg msgWaiting, Waiting modelWaiting ) ->
            Waiting.update msgWaiting modelWaiting
                |> updateWith Waiting WaitingMsg

        _ ->
            ( model, Cmd.none )


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg ( subModel, subCmd ) =
    ( toModel subModel, Cmd.map toMsg subCmd )


messageDecoder : Decoder (Model -> Model)
messageDecoder =
    let
        waiting state model =
            case model of
                Waiting modelWaiting ->
                    Waiting (Waiting.updateState state modelWaiting)

                _ ->
                    Waiting (Waiting.init state)
    in
    Decode.map waiting Waiting.decoder



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "w-screen h-screen flex justify-center items-center" ]
        [ case model of
            Connecting ->
                div [ class "text-8xl" ] [ text "Connecting" ]

            Waiting modelWaiting ->
                Html.map WaitingMsg (Waiting.view modelWaiting)

            Error s ->
                div [ class "text-4xl text-red-500" ]
                    [ text <| "Error: " ++ s ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    WebSocket.subscribe WebSocketEvent
