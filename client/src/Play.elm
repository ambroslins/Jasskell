module Play exposing (main)

import Browser
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Pipeline as Decode
import Play.AsGuest as AsGuest
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
    | AsGuest AsGuest.Model
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
    | AsGuestMsg AsGuest.Msg


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

        ( AsGuestMsg msgGuest, AsGuest modelGuest ) ->
            AsGuest.update msgGuest modelGuest
                |> updateWith AsGuest AsGuestMsg

        _ ->
            ( model, Cmd.none )


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg ( subModel, subCmd ) =
    ( toModel subModel, Cmd.map toMsg subCmd )


messageDecoder : Decoder (Model -> Model)
messageDecoder =
    let
        guest state model =
            case model of
                AsGuest modelGuest ->
                    AsGuest (AsGuest.updateState state modelGuest)

                _ ->
                    AsGuest (AsGuest.init state)
    in
    Decode.field "status" Decode.string
        |> Decode.andThen
            (\status ->
                case status of
                    "guest" ->
                        Decode.map guest AsGuest.decode

                    _ ->
                        Decode.fail ("invalid status: " ++ status)
            )



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "w-screen h-screen flex justify-center items-center" ]
        [ case model of
            Connecting ->
                div [ class "text-8xl" ] [ text "Connecting" ]

            AsGuest modelGuest ->
                Html.map AsGuestMsg (AsGuest.view modelGuest)

            Error s ->
                div [ class "text-4xl text-red-500" ]
                    [ text <| "Error: " ++ s ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    WebSocket.subscribe WebSocketEvent
