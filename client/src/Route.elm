module Route exposing (Route(..), href, parse, push, toString)

import Browser.Navigation as Nav
import Html exposing (Attribute)
import Html.Attributes as Attributes
import TableID exposing (TableID)
import Url exposing (Url)
import Url.Builder as Builder
import Url.Parser as Parser exposing ((</>), s)


type Route
    = Home
    | Play TableID


push : Nav.Key -> Route -> Cmd msg
push key route =
    Nav.pushUrl key (toString route)


toString : Route -> String
toString route =
    Builder.absolute (toPieces route) []


href : Route -> Attribute msg
href =
    Attributes.href << toString


toPieces : Route -> List String
toPieces route =
    case route of
        Home ->
            []

        Play table ->
            [ "play", TableID.toString table ]


parse : Url -> Maybe Route
parse =
    Parser.parse <|
        Parser.oneOf
            [ Parser.map Home Parser.top
            , Parser.map Play <| s "play" </> TableID.parse
            ]
