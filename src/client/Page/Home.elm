module Page.Home exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Result
import WebSocket



-- MODEL


type alias Model =
    String


init : Model
init =
    ""


view : Model -> Html Msg
view model =
    div [ class "home" ] [ button [ onClick NewGame ] [ text "New Game" ], input [ placeholder "GameID", value model, onInput ChangeGameID ] [], button [ onClick Join ] [ text "Join" ] ]



-- UPDATE


type Msg
    = NewGame
    | ChangeGameID String
    | Join
    | OnOpen


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewGame ->
            ( model, Http.post { body = Http.emptyBody, url = "/newgame", expect = Http.expectJson (ChangeGameID << Result.withDefault "") Decode.string } )

        ChangeGameID s ->
            ( s, Cmd.none )

        Join ->
            ( model, WebSocket.connect "ws://localhost:9000" )

        OnOpen ->
            ( model, WebSocket.send (Encode.object [ ( "gameID", Encode.string model ), ( "username", Encode.string "username" ) ]) )
