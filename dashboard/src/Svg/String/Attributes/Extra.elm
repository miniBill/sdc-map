module Svg.String.Attributes.Extra exposing (color, em, percent, points, px, translate, viewBox)

import Color exposing (Color)
import Svg.String exposing (Attribute)
import Svg.String.Attributes as SAttrs


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


points : List ( Float, Float ) -> String
points values =
    values
        |> List.map (\( x, y ) -> pointToString x y)
        |> String.join " "


pointToString : Float -> Float -> String
pointToString x y =
    String.fromFloat x ++ "," ++ String.fromFloat y


translate : Float -> Float -> String
translate dx dy =
    "translate(" ++ pointToString dx dy ++ ")"
