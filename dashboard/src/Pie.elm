module Pie exposing (view)

import Array exposing (Array)
import Color exposing (Color)
import Color.Oklch
import Html exposing (Html)
import Path
import Shape exposing (defaultPieConfig)
import Svg.String as Svg exposing (Svg, g, svg, text, text_)
import Svg.String.Attributes as SAttrs exposing (dy, fill, fontSize, stroke, transform)
import Svg.String.Attributes.Extra as EAttrs


w : Float
w =
    900


h : Float
h =
    900


radius : Float
radius =
    min w h / 2


pieSlice : Array Color -> Int -> Shape.Arc -> Svg msg
pieSlice colors index datum =
    Svg.node "path"
        [ SAttrs.attribute "d" (Path.toString (Shape.arc datum))
        , fill <| EAttrs.color <| Maybe.withDefault Color.black <| Array.get index colors
        , stroke <| EAttrs.color Color.white
        ]
        []


pieLabel : Shape.Arc -> ( String, Float ) -> Svg msg
pieLabel slice ( label, _ ) =
    let
        ( x, y ) =
            Shape.centroid
                { slice
                    | innerRadius = radius * 2 / 3
                    , outerRadius = radius * 2 / 3
                }
    in
    text_
        [ transform <| EAttrs.translate x y
        , dy (EAttrs.em 0.35)
        , SAttrs.attribute "text-anchor" "middle"
        ]
        [ text label ]


view : List ( String, Float ) -> Html msg
view model =
    let
        pieData : List Shape.Arc
        pieData =
            model
                |> List.map Tuple.second
                |> Shape.pie { defaultPieConfig | outerRadius = radius }

        colors : Array Color
        colors =
            let
                length : Int
                length =
                    List.length model
            in
            List.range 0 (length - 1)
                |> List.map
                    (\i ->
                        Color.Oklch.oklch 0.7 0.1 (toFloat i / toFloat length)
                            |> Color.Oklch.toColor
                    )
                |> Array.fromList
    in
    Svg.toHtml <|
        svg [ EAttrs.viewBox 0 0 w h ]
            [ g
                [ transform <| EAttrs.translate (w / 2) (h / 2)
                , fontSize (EAttrs.px <| min w h / 20)
                ]
                [ g [] <| List.indexedMap (pieSlice colors) pieData
                , g [] <| List.map2 pieLabel pieData model
                ]
            ]
