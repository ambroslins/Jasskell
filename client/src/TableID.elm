module TableID exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Url.Parser as Parser exposing (Parser)


type TableID
    = TableID String


toString : TableID -> String
toString (TableID tableID) =
    tableID


parse : Parser (TableID -> a) a
parse =
    Parser.map TableID Parser.string


decode : Decoder TableID
decode =
    Decode.map TableID Decode.string
