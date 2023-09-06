module Pie exposing (view)

import Array exposing (Array)
import Color exposing (Color)
import Color.Oklch
import Path
import Shape exposing (defaultPieConfig)
import TypedSvg exposing (g, svg, text_)
import TypedSvg.Attributes exposing (dy, fill, fontSize, stroke, textAnchor, transform, viewBox)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (AnchorAlignment(..), Paint(..), Transform(..), em, px)


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
    Path.element (Shape.arc datum)
        [ fill <| Paint <| Maybe.withDefault Color.black <| Array.get index colors
        , stroke <| Paint Color.white
        ]


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
        [ transform [ Translate x y ]
        , dy (em 0.35)
        , textAnchor AnchorMiddle
        ]
        [ text label ]


view : List ( String, Float ) -> Svg msg
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
    svg [ viewBox 0 0 w h ]
        [ g
            [ transform [ Translate (w / 2) (h / 2) ]
            , fontSize (px <| min w h / 20)
            ]
            [ g [] <| List.indexedMap (pieSlice colors) pieData
            , g [] <| List.map2 pieLabel pieData model
            ]
        ]
