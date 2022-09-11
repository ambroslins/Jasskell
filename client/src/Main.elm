module Main exposing (..)

import Browser exposing (Document, UrlRequest(..))
import Browser.Navigation as Nav
import Home
import Html
import Maybe exposing (withDefault)
import Play
import Result exposing (fromMaybe)
import Route exposing (Route(..))
import Url exposing (Url)



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        , onUrlChange = UrlChange
        , onUrlRequest = UrlRequest
        }



-- MODEL


type alias Model =
    { page : Page
    , key : Nav.Key
    }


type Page
    = Home Home.Model
    | Play Play.Model


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init () url key =
    let
        route =
            withDefault Route.Home <| Route.parse url

        ( model, cmd ) =
            Home.init ()
    in
    changeRoute route { page = Home model, key = key }



-- UPDATE


type Msg
    = UrlChange Url
    | UrlRequest UrlRequest
    | HomeMsg Home.Msg
    | PlayMsg Play.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case ( message, model.page ) of
        ( UrlRequest urlRequest, _ ) ->
            let
                _ =
                    Debug.log "url request" urlRequest
            in
            case urlRequest of
                Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                External url ->
                    ( model, Nav.load url )

        ( UrlChange url, _ ) ->
            let
                _ =
                    Debug.log "url change: " url

                route =
                    withDefault Route.Home <| Route.parse url
            in
            changeRoute route model

        ( HomeMsg msg, Home page ) ->
            Home.update model.key msg page
                |> updatePage Home HomeMsg model

        ( PlayMsg msg, Play page ) ->
            Play.update msg page
                |> updatePage Play PlayMsg model

        ( _, _ ) ->
            ( model, Cmd.none )


updatePage : (page -> Page) -> (msg -> Msg) -> Model -> ( page, Cmd msg ) -> ( Model, Cmd Msg )
updatePage toPage toMsg model ( page, cmd ) =
    ( { model | page = toPage page }
    , Cmd.map toMsg cmd
    )


changeRoute : Route -> Model -> ( Model, Cmd Msg )
changeRoute route model =
    case route of
        Route.Home ->
            updatePage Home HomeMsg model <| Home.init ()

        Route.Play tableID ->
            updatePage Play PlayMsg model <| Play.init tableID



-- VIEW


view : Model -> Document Msg
view model =
    let
        viewPage =
            case model.page of
                Home page ->
                    Html.map HomeMsg <| Home.view page

                Play page ->
                    Html.map PlayMsg <| Play.view page
    in
    { title = "Jasskell"
    , body = [ viewPage ]
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        Home page ->
            Sub.map HomeMsg <| Home.subscriptions page

        Play page ->
            Sub.map PlayMsg <| Play.subscriptions page
