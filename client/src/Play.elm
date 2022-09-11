module Play exposing (Model, Msg, init, subscriptions, update, view)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import TableID exposing (TableID)



-- MODEL


type alias Model =
    { tableID : TableID }


init : TableID -> ( Model, Cmd Msg )
init tableID =
    ( { tableID = tableID }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Create


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Create ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "w-screen h-screen flex justify-center items-center" ]
        [ div [ class "text-8xl" ]
            [ text <| TableID.toString model.tableID ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
