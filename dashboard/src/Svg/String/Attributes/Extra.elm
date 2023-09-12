module Svg.String.Attributes.Extra exposing (color, em, href, percent, points, px, translate, viewBox)

import Color exposing (Color)
import Svg.String as Svg exposing (Attribute)
import Svg.String.Attributes as SAttrs


href : String -> Attribute msg
href value =
    SAttrs.xlinkHref value


percent : Float -> String
percent value =
    String.fromFloat value ++ "%"


px : Float -> String
px value =
    String.fromFloat value ++ "px"


em : Float -> String
em value =
    String.fromFloat value ++ "em"


color : Color -> String
color value =
    Color.toCssString value


viewBox : Float -> Float -> Float -> Float -> Attribute msg
viewBox x y width height =
    [ x, y, width, height ]
        |> List.map String.fromFloat
        |> String.join " "
        |> SAttrs.viewBox


points : List ( Float, Float ) -> Svg.Attribute msg
points values =
    values
        |> List.map (\( x, y ) -> pointToString x y)
        |> String.join " "
        |> SAttrs.attribute "points"


pointToString : Float -> Float -> String
pointToString x y =
    String.fromFloat x ++ "," ++ String.fromFloat y


translate : Float -> Float -> String
translate dx dy =
    "translate(" ++ pointToString dx dy ++ ")"
