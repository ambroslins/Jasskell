module Circle exposing (..)

import Html exposing (Attribute, Html, div)
import Html.Attributes exposing (class, style)
import Html.Keyed as Keyed


type alias Circle msg =
    { radius : Float -- in rem
    , elementSize : { width : Float, height : Float }
    , offset : Int
    , center : Html msg
    , elements :
        List
            { attributes : List (Attribute msg)
            , key : String
            , child : Html msg
            }
    }


view : Circle msg -> Html msg
view { radius, elementSize, offset, center, elements } =
    let
        viewElement i { attributes, key, child } =
            let
                angle =
                    toFloat (i - offset) * 2 * pi / toFloat (List.length elements)

                x =
                    negate <| radius * sin angle

                y =
                    radius * cos angle
            in
            Keyed.node "div"
                (attributes
                    ++ [ style "grid-column" "1"
                       , style "grid-row" "1"
                       , style "width" <| toRem elementSize.width
                       , style "height" <| toRem elementSize.height
                       , style "transform" <|
                            String.concat
                                [ "translate(", toRem x, ", ", toRem y, ")" ]
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
