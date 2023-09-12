module Map exposing (map, view)

import Color exposing (Color)
import Dict
import Element exposing (Element, text)
import FNV1a
import GeoJson exposing (Geometry(..), Position)
import List.Extra
import RemoteData exposing (RemoteData(..))
import Set exposing (Set)
import Svg.String as Svg exposing (Svg)
import Svg.String.Attributes as SAttrs
import Svg.String.Attributes.Extra as EAttrs
import Theme
import Types exposing (Model, Msg(..))


view : Model -> List { country : String, location : String, names : List String } -> Element Msg
view model locations =
    Theme.column []
        [ map model
            locations
            [ SAttrs.attribute "style" <|
                "width: calc(100vw - "
                    ++ EAttrs.px (Theme.rythm * 4 + 4)
                    ++ ")"
            ]
            |> Svg.toHtml
            |> Element.html
        , Theme.button []
            { onPress = Just Download
            , label = text "Download"
            }
        ]


map : Model -> List { country : String, location : String, names : List String } -> List (Svg.Attribute msg) -> Svg.Html msg
map model locations attrs =
    let
        east : Float
        east =
            Tuple.first <| winkelTripel ( 180, 0, 0 )

        north : Float
        north =
            Tuple.second <| winkelTripel ( 0, 90, 0 )
    in
    [ viewBackground east
    , viewCountriesBorders model
    , viewLocationDots model locations
    ]
        |> Svg.svg
            (EAttrs.viewBox -east -north (2 * east) (2 * north)
                :: attrs
            )


viewBackground : Float -> Svg msg
viewBackground east =
    let
        winkelWidth : Float
        winkelWidth =
            2 * east

        imageWidth : number
        imageWidth =
            2058

        imageHeight : number
        imageHeight =
            1262

        imageRatio : Float
        imageRatio =
            imageWidth / imageHeight

        height : Float
        height =
            winkelWidth / imageRatio
    in
    Svg.image
        [ SAttrs.href "world.jpg"
        , SAttrs.width <| EAttrs.percent 100
        , SAttrs.height <| EAttrs.px height
        , SAttrs.x <| EAttrs.percent -50
        , SAttrs.y <| (EAttrs.px <| -height / 2)
        ]
        []


viewLocationDots : Model -> List { country : String, location : String, names : List String } -> Svg msg
viewLocationDots model locations =
    locations
        |> List.filterMap
            (\input ->
                case Types.findPosition model input of
                    Ok pos ->
                        Just <| viewLocationDot ( winkelTripelFlip pos, input.names )

                    Err _ ->
                        case Types.findPosition model { input | location = "" } of
                            Ok pos ->
                                Just <| viewLocationDot ( winkelTripelFlip pos, input.names )

                            Err _ ->
                                Nothing
            )
        |> Svg.g
            [ SAttrs.fill (EAttrs.color Color.red)
            ]


viewLocationDot : ( ( Float, Float ), List String ) -> Svg msg
viewLocationDot ( ( x, y ), names ) =
    Svg.circle
        [ SAttrs.cx (EAttrs.px x)
        , SAttrs.cy (EAttrs.px y)
        , SAttrs.r (EAttrs.percent 0.2)
        ]
        [ Svg.title [] [ Svg.text <| String.join ", " names ] ]


viewCountriesBorders : Model -> Svg msg
viewCountriesBorders ({ geoJsonData } as model) =
    let
        countries : Set String
        countries =
            Types.validInputs model
                |> List.map .country
                |> Set.fromList
    in
    geoJsonData
        |> Dict.toList
        |> List.filterMap
            (\( country, data ) ->
                if Set.member country countries then
                    case data of
                        Success countryLocations ->
                            List.Extra.find
                                (\location -> location.name == country)
                                countryLocations
                                |> Maybe.map (\{ geometry } -> viewCountryBorders country geometry)

                        _ ->
                            Nothing

                else
                    Nothing
            )
        |> Svg.g
            [ SAttrs.stroke <| EAttrs.color Color.black
            , SAttrs.strokeWidth <| EAttrs.percent 0.02
            ]


viewCountryBorders : String -> Geometry -> Svg msg
viewCountryBorders country geometry =
    let
        go : Geometry -> List (Svg msg)
        go child =
            -- elm-review: IGNORE TCO
            case child of
                Point _ ->
                    []

                MultiPoint _ ->
                    []

                LineString _ ->
                    [ Svg.text_ [] [ Svg.text "branch 'LineString _' not implemented" ] ]

                MultiLineString _ ->
                    [ Svg.text_ [] [ Svg.text "branch 'MultiLineString _' not implemented" ] ]

                Polygon polygons ->
                    List.map
                        viewPolygon
                        polygons

                MultiPolygon polygons ->
                    polygons
                        |> List.concat
                        |> List.map viewPolygon

                GeometryCollection children ->
                    List.concatMap go children
    in
    Svg.g
        [ SAttrs.fill <| EAttrs.color <| countryColor country
        , SAttrs.id country
        ]
        (go geometry)


viewPolygon : List Position -> Svg msg
viewPolygon points =
    let
        roundish2 : Float -> ( Float, Float ) -> ( Float, Float )
        roundish2 digits ( x, y ) =
            ( roundish digits x
            , roundish digits y
            )

        roundish : Float -> Float -> Float
        roundish digits v =
            let
                k : Float
                k =
                    10 ^ digits
            in
            ((v * k)
                |> round
                |> toFloat
            )
                / k

        ( projected, count ) =
            case
                points
                    |> List.map winkelTripelFlip
            of
                [] ->
                    ( [], 0 )

                head :: tail ->
                    List.foldl
                        (\e ( last, acc, countAcc ) ->
                            if e == last then
                                ( last, acc, countAcc )

                            else
                                ( e
                                , last :: acc
                                , if roundish2 2 e == roundish2 2 last then
                                    countAcc

                                  else
                                    countAcc + 1
                                )
                        )
                        ( head, [], 0 )
                        tail
                        |> (\( last, acc, countAcc ) ->
                                ( last :: acc, countAcc )
                           )
    in
    if count >= 5 then
        Svg.polygon
            [ SAttrs.points <| EAttrs.points projected
            ]
            [-- Html.text <| String.fromInt count
            ]

    else
        Svg.text ""


countryColor : String -> Color
countryColor country =
    let
        hash : Int
        hash =
            FNV1a.hash country

        ( r, g, b ) =
            ( div (hash // (256 * 256))
            , div (hash // 256)
            , div (hash // 1)
            )

        div : Int -> Float
        div n =
            toFloat (modBy 256 n) / 255
    in
    Color.rgba r g b 0.4


winkelTripelFlip : Position -> ( Float, Float )
winkelTripelFlip pos =
    let
        ( x, y ) =
            winkelTripel pos
    in
    ( x, -y )


winkelTripel : Position -> ( Float, Float )
winkelTripel ( long, lat, _ ) =
    let
        lambda : Float
        lambda =
            degrees long

        φ : Float
        φ =
            degrees lat

        α : Float
        α =
            acos (cos φ * cos (lambda / 2))

        φ_1 : Float
        φ_1 =
            acos (2 / pi)

        x : Float
        x =
            0.5 * (lambda * cos φ_1 + 2 * (cos φ * sin (lambda / 2)) / sinc α)

        y : Float
        y =
            0.5 * (φ + sin φ / sinc α)
    in
    ( x * 1000, y * 1000 )


sinc : Float -> Float
sinc x =
    if x == 0 then
        1

    else
        sin x / x
