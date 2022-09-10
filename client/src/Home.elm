module Home exposing (Model, Msg, init, subscriptions, update, view)

import Browser.Navigation as Nav
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Route



-- MODEL


type alias Model =
    ()


init : () -> ( Model, Cmd Msg )
init () =
    ( (), Cmd.none )



-- UPDATE


type Msg
    = Create


update : Nav.Key -> Msg -> Model -> ( Model, Cmd Msg )
update key msg model =
    case msg of
        Create ->
            ( model, Nav.pushUrl key "/play/foo" )



-- VIEW


view : Model -> Html Msg
view () =
    div [ class "w-screen h-screen flex justify-center items-center" ]
        [ button
            [ onClick Create
            , class "border-4 border-lime-200 hover:bg-lime-200 rounded-xl text-6xl"
            ]
            [ text "Create a Table" ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
