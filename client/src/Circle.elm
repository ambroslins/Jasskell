module Circle exposing (..)

import Html exposing (Attribute, Html, div)
import Html.Attributes exposing (class, style)
import Html.Keyed as Keyed


type alias Circle msg =
    { radius : Float -- in rem
    , elementSize : { width : Float, height : Float }
    , center : Html msg
    , elements :
        List
            { attributes : List (Attribute msg)
            , key : String
            , child : Html msg
            }
    }


view : Circle msg -> Html msg
view { radius, elementSize, center, elements } =
    let
        count =
            toFloat (List.length elements)

        viewElement i { attributes, key, child } =
            let
                angle =
                    2 * pi * (toFloat i / count + 0.25)
            in
            Keyed.node "div"
                (attributes
                    ++ [ style "grid-column" "1"
                       , style "grid-row" "1"
                       , style "width" <| toRem elementSize.width
                       , style "height" <| toRem elementSize.height
                       , style "transform" <|
                            String.join " "
                                [ rotate angle, translate radius, rotate -angle ]
                       ]
                )
                [ ( key, child ) ]
    in
    div
        [ class "justify-center items-center"
        , style "display" "grid"
        , style "width" <| toRem (2 * radius + elementSize.width)
        , style "height" <| toRem (2 * radius + elementSize.height)
        ]
    <|
        center
            :: List.indexedMap viewElement elements


toRem : Float -> String
toRem f =
    String.fromFloat f ++ "rem"


translate : Float -> String
translate distance =
    "translate(" ++ toRem distance ++ ")"


toRad : Float -> String
toRad f =
    String.fromFloat f ++ "rad"


rotate : Float -> String
rotate angle =
    "rotate(" ++ toRad angle ++ ")"
