module Play exposing (Model, Msg, init, subscriptions, update, view)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Route
import TableID exposing (TableID)
import WebSocket



-- MODEL


type State
    = Connecting
    | Connected String
    | Error


type alias Model =
    { state : State }


init : TableID -> ( Model, Cmd Msg )
init tableID =
    ( { state = Connecting }
    , WebSocket.open <| Route.toString (Route.Play tableID)
    )



-- UPDATE


type Msg
    = WebSocketEvent WebSocket.Event


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WebSocketEvent event ->
            case event of
                WebSocket.Open ->
                    ( { model | state = Connected "" }, Cmd.none )

                WebSocket.Message message ->
                    ( { model | state = Connected message }, Cmd.none )

                WebSocket.Error ->
                    ( { model | state = Error }, Cmd.none )

                WebSocket.Close ->
                    ( { model | state = Error }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "w-screen h-screen flex justify-center items-center" ]
        [ div [ class "text-8xl" ]
            [ text <|
                case model.state of
                    Connecting ->
                        "Connecting"

                    Connected s ->
                        "Connected: " ++ s

                    Error ->
                        "Error"
            ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    WebSocket.subscribe WebSocketEvent
