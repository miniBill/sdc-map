module Dashboard exposing (Country, Location, Model, Msg, init, update, view)

import Color exposing (Color)
import Dict exposing (Dict)
import Element exposing (Element, alignTop, centerX, el, fill, paragraph, px, rgb, shrink, text, width, wrappedRow)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import FNV1a
import GeoJson exposing (GeoJsonObject(..), Geometry(..), Position)
import Html
import Html.Attributes
import Http
import Http.Tasks
import Json.Decode exposing (Decoder)
import List.Extra
import Pie
import RemoteData exposing (RemoteData(..), WebData)
import Result.Extra
import Round
import Set exposing (Set)
import Task
import Theme
import Theme.Dashboard as Theme
import TypedSvg as Svg
import TypedSvg.Attributes as SAttrs
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types as STypes
import Types exposing (Input)


type Msg
    = InvalidCaptchas (Set String)
    | GotIndexData (Result Http.Error (Dict Country { threeLetterCode : String, level : Int }))
    | GotCapitalsData (Result Http.Error (Dict Country Position))
    | GotGeoJson Country (Result (Maybe Http.Error) (List Location))
    | ReloadCountry Country


type alias Model =
    { invalidCaptchas : Set String
    , inputs : List Input
    , indexData : WebData (Dict Country { threeLetterCode : String, level : Int })
    , geoJsonData : Dict Country (RemoteData (Maybe Http.Error) (List Location))
    , capitals : WebData (Dict Country Position)
    }


type alias Location =
    { name : String
    , alternativeNames : List String
    , geometry : Geometry
    , center : Position
    }


init : List Input -> ( Model, Cmd Msg )
init inputs =
    ( { inputs = inputs
      , invalidCaptchas = Set.empty
      , geoJsonData = Dict.empty
      , indexData = Loading
      , capitals = Loading
      }
    , Cmd.batch
        [ Http.get
            { url = "/geodata/index.json"
            , expect = Http.expectJson GotIndexData indexDataDecoder
            }
        , Http.get
            { url = "/geodata/capitals.geojson"
            , expect = Http.expectJson GotCapitalsData capitalsDataDecoder
            }
        ]
    )


capitalsDataDecoder : Decoder (Dict Country Position)
capitalsDataDecoder =
    GeoJson.decoder
        |> Json.Decode.andThen
            (\( geoJson, _ ) ->
                case geoJson of
                    Geometry _ ->
                        Json.Decode.fail "Unexpected geometry"

                    Feature _ ->
                        Json.Decode.fail "Unexpected feature"

                    FeatureCollection objects ->
                        objects
                            |> Result.Extra.combineMap
                                (\object ->
                                    case
                                        ( object.geometry
                                        , Json.Decode.decodeValue
                                            (Json.Decode.field "country" Json.Decode.string)
                                            object.properties
                                        )
                                    of
                                        ( Just (Point position), Ok country ) ->
                                            Ok ( country, position )

                                        ( _, Ok _ ) ->
                                            Err "Unexpected feature"

                                        ( _, Err e ) ->
                                            Err (Json.Decode.errorToString e)
                                )
                            |> Result.mapError Json.Decode.fail
                            |> Result.map (Dict.fromList >> Json.Decode.succeed)
                            |> Result.Extra.merge
            )


indexDataDecoder : Decoder (Dict String { threeLetterCode : String, level : Int })
indexDataDecoder =
    let
        elementDecoder : String -> Decoder { threeLetterCode : String, level : Int }
        elementDecoder rawString =
            case String.split "_" rawString of
                [ threeLetterCode, _, levelString ] ->
                    case String.toInt levelString of
                        Nothing ->
                            Json.Decode.fail <| "Could not parse '" ++ levelString ++ "' as an integer"

                        Just level ->
                            Json.Decode.succeed
                                { threeLetterCode = threeLetterCode
                                , level = level
                                }

                _ ->
                    Json.Decode.fail <| "Could not split '" ++ rawString ++ "' in three parts"
    in
    Json.Decode.dict
        (Json.Decode.andThen
            elementDecoder
            Json.Decode.string
        )


type alias Country =
    String


view : Model -> Element Msg
view model =
    let
        { onMap, locations, map } =
            viewOnMap model
    in
    [ card "Statistics by country" <| viewByCountry model
    , Theme.column [ alignTop ]
        [ card "Captchas" <| viewCaptchas model
        , card "GeoData" <| viewGeoDataTable model
        ]
    , card "On map" onMap
    , card "Locations" locations
    , card "Map" map
    ]
        |> wrappedRow [ Theme.spacing ]


validInputs : Model -> List Input
validInputs model =
    model.inputs
        |> List.filter (\{ captcha } -> not (Set.member (String.toLower captcha) model.invalidCaptchas))


viewOnMap :
    Model
    ->
        { onMap : Element msg
        , locations : Element msg
        , map : Element msg
        }
viewOnMap model =
    let
        filteredInputs : List Input
        filteredInputs =
            validInputs model
                |> List.filter
                    (\{ nameOnMap } ->
                        nameOnMap == Just True
                    )

        locations : List { country : String, location : String, names : List String }
        locations =
            filteredInputs
                |> List.Extra.gatherEqualsBy (\{ country, location } -> ( country, location ))
                |> List.map
                    (\( { country, location, name }, others ) ->
                        { country = country
                        , location = location
                        , names = name :: List.map .name others
                        }
                    )
                |> List.sortBy (\{ country, location } -> ( country, location ))
    in
    { onMap =
        Theme.table [ alignTop ]
            { data = filteredInputs
            , columns =
                [ Theme.tableColumnText "Name" .name
                , Theme.tableColumnText "Country" .country
                , Theme.tableColumnText "Location" .location
                ]
            }
    , locations =
        Theme.table []
            { data = locations
            , columns =
                [ Theme.tableColumnText "Country" .country
                , Theme.tableColumnText "Location" .location
                , Theme.tableColumnText "Found?" <|
                    \input ->
                        case findPosition model input of
                            Ok ( lon, lat, _ ) ->
                                let
                                    i : String
                                    i =
                                        [ lon, lat ]
                                            |> List.map (Round.round 4)
                                            |> String.join ", "
                                in
                                "(" ++ i ++ ")"

                            Err e ->
                                "Err: " ++ e
                ]
            }
    , map = viewMap model locations
    }


viewGeoDataTable : Model -> Element Msg
viewGeoDataTable model =
    let
        needed : List String
        needed =
            model.geoJsonData
                |> Dict.toList
                |> List.filterMap
                    (\( country, geoJson ) ->
                        case geoJson of
                            Failure (Just (Http.BadStatus 404)) ->
                                model.indexData
                                    |> RemoteData.toMaybe
                                    |> Maybe.withDefault Dict.empty
                                    |> Dict.get country
                                    |> Maybe.map
                                        (\{ threeLetterCode, level } ->
                                            List.range 0 (level - 2)
                                                |> List.map
                                                    (geodataUrl threeLetterCode)
                                        )

                            _ ->
                                Nothing
                    )
                |> List.concat

        columns : List (Maybe (Theme.Column ( Country, RemoteData (Maybe Http.Error) (List Location) ) Msg))
        columns =
            [ Just <| Theme.tableColumnText "Country" Tuple.first
            , Just <|
                Theme.tableColumnText "Status" <|
                    \( _, geoJson ) ->
                        case geoJson of
                            Failure (Just e) ->
                                httpErrorToString e

                            Failure Nothing ->
                                "Not found in index"

                            Loading ->
                                "Loading..."

                            Success _ ->
                                "Loaded"
            , commandsColumn
            ]

        commandsColumn :
            Maybe
                (Theme.Column
                    ( Country
                    , RemoteData (Maybe Http.Error) (List Location)
                    )
                    Msg
                )
        commandsColumn =
            if List.isEmpty needed then
                Nothing

            else
                { header = "Commands"
                , width = shrink
                , view =
                    \( country, geoJson ) ->
                        case geoJson of
                            Failure (Just _) ->
                                ( []
                                , Theme.button []
                                    { label =
                                        case
                                            model.indexData
                                                |> RemoteData.toMaybe
                                                |> Maybe.withDefault Dict.empty
                                                |> Dict.get country
                                        of
                                            Nothing ->
                                                text "Reload"

                                            Just { threeLetterCode } ->
                                                text <| "Reload " ++ threeLetterCode
                                    , onPress = Just (ReloadCountry country)
                                    }
                                )

                            _ ->
                                ( [], Element.none )
                }
                    |> Just
    in
    Theme.column [ alignTop ]
        [ case model.indexData of
            Loading ->
                text "Loading index data..."

            Failure e ->
                text <| httpErrorToString e

            Success _ ->
                text "Loaded index data."
        , Theme.table []
            { data = Dict.toList model.geoJsonData
            , columns = List.filterMap identity columns
            }
        , if List.isEmpty needed then
            Element.none

          else
            paragraph [] [ text <| String.join " " <| "make -j" :: needed ]
        ]


geodataUrl : String -> Int -> String
geodataUrl threeLetterCode lvl =
    "geodata/gadm41_"
        ++ threeLetterCode
        ++ "_"
        ++ String.fromInt lvl
        ++ ".json"


viewMap : Model -> List { country : String, location : String, names : List String } -> Element msg
viewMap model locations =
    let
        east : Float
        east =
            Tuple.first <| winkelTripel ( 180, 0, 0 )

        north : Float
        north =
            Tuple.second <| winkelTripel ( 0, 90, 0 )

        winkelWidth : Float
        winkelWidth =
            2 * east

        background : Svg msg
        background =
            let
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
                , SAttrs.width <| STypes.percent 100
                , SAttrs.height <| STypes.px height
                , SAttrs.x <| STypes.percent -50
                , SAttrs.y <| (STypes.px <| -height / 2)
                ]
                []

        viewPoint : ( ( Float, Float ), List String ) -> Svg msg
        viewPoint ( ( x, y ), names ) =
            Svg.circle
                [ SAttrs.cx (STypes.px x)
                , SAttrs.cy (STypes.px y)
                , SAttrs.r (STypes.px 0.01)
                , SAttrs.fill (STypes.Paint Color.red)
                ]
                [ Svg.title [] [ Html.text <| String.join ", " names ] ]

        countries : List Country
        countries =
            locations
                |> List.map .country
                |> Set.fromList
                |> Set.toList

        countriesBorders : Svg msg
        countriesBorders =
            countries
                |> List.filterMap
                    (\country ->
                        case Dict.get country model.geoJsonData of
                            Just (Success countryLocations) ->
                                List.Extra.find
                                    (\location -> location.name == country)
                                    countryLocations
                                    |> Maybe.map (\{ geometry } -> viewCountryBorders country geometry)

                            _ ->
                                Nothing
                    )
                |> Svg.g []

        locationDots : Svg msg
        locationDots =
            locations
                |> List.filterMap
                    (\input ->
                        case findPosition model input of
                            Ok pos ->
                                Just <| viewPoint ( winkelTripelFlip pos, input.names )

                            Err _ ->
                                case findPosition model { input | location = "" } of
                                    Ok pos ->
                                        Just <| viewPoint ( winkelTripelFlip pos, input.names )

                                    Err _ ->
                                        Nothing
                    )
                |> Svg.g []
    in
    [ background
    , countriesBorders
    , locationDots
    ]
        |> Svg.svg
            [ Html.Attributes.style "width"
                ("calc(100vw - " ++ String.fromInt (Theme.rythm * 4 + 4) ++ "px)")
            , SAttrs.viewBox -east -north (2 * east) (2 * north)
            ]
        |> Element.html


viewCountryBorders : String -> Geometry -> Svg msg
viewCountryBorders country geometry =
    let
        viewPoint p =
            let
                ( cx, cy ) =
                    winkelTripelFlip p
            in
            Svg.circle
                [ SAttrs.cx <| STypes.px cx
                , SAttrs.cy <| STypes.px cy
                , SAttrs.r <| STypes.px 10
                , SAttrs.fill <| STypes.Paint <| countryColor country
                ]
                []

        go : Geometry -> List (Svg msg)
        go child =
            case child of
                Point p ->
                    [ viewPoint p ]

                MultiPoint points ->
                    List.map
                        viewPoint
                        points

                LineString _ ->
                    [ Svg.text_ [] [ Html.text "branch 'LineString _' not implemented" ] ]

                MultiLineString _ ->
                    [ Svg.text_ [] [ Html.text "branch 'MultiLineString _' not implemented" ] ]

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
        [ SAttrs.fill <| STypes.Paint <| countryColor country
        , SAttrs.stroke <| STypes.Paint Color.black
        , SAttrs.strokeWidth <| STypes.px 0.0015
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
            [ SAttrs.points projected
            ]
            [-- Html.text <| String.fromInt count
            ]

    else
        Html.text ""


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
        lambda =
            degrees long

        φ =
            degrees lat

        α =
            acos (cos φ * cos (lambda / 2))

        φ_1 =
            acos (2 / pi)

        x =
            0.5 * (lambda * cos φ_1 + 2 * (cos φ * sin (lambda / 2)) / sinc α)

        y =
            0.5 * (φ + sin φ / sinc α)
    in
    ( x, y )


sinc : Float -> Float
sinc x =
    if x == 0 then
        1

    else
        sin x / x


findPosition : Model -> { a | country : String, location : String } -> Result String Position
findPosition model { country, location } =
    if String.isEmpty location then
        case model.capitals of
            Loading ->
                Err "Loading"

            Failure _ ->
                Err "Failure"

            Success capitals ->
                case Dict.get country capitals of
                    Nothing ->
                        Err "Missing (capitals)"

                    Just capital ->
                        Ok capital

    else
        case Dict.get (normalizeCountry country) model.geoJsonData of
            Nothing ->
                Err "Missing (data)"

            Just Loading ->
                Err "Loading"

            Just (Failure Nothing) ->
                Err "Missing (index)"

            Just (Failure _) ->
                Err "Failure"

            Just (Success locations) ->
                case locations of
                    [] ->
                        Err "No jsons loaded"

                    _ ->
                        let
                            normalized : String
                            normalized =
                                normalize location

                            normalize : String -> String
                            normalize s =
                                String.toLower s |> String.replace " " ""
                        in
                        locations
                            |> List.Extra.findMap
                                (\loc ->
                                    if
                                        (normalize loc.name == normalized)
                                            || List.member
                                                normalized
                                                (List.map normalize loc.alternativeNames)
                                    then
                                        Just (Ok loc.center)

                                    else
                                        Nothing
                                )
                            |> Maybe.withDefault (Err "Not found")


httpErrorToString : Http.Error -> String
httpErrorToString error =
    case error of
        Http.BadUrl url ->
            "BadUrl " ++ url

        Http.Timeout ->
            "Timeout"

        Http.NetworkError ->
            "NetworkError"

        Http.BadStatus status ->
            "BadStatus " ++ String.fromInt status

        Http.BadBody errorMessage ->
            "BadBody " ++ errorMessage


viewByCountry : Model -> Element Msg
viewByCountry model =
    let
        data : List { country : String, count : Int }
        data =
            model
                |> validInputs
                |> List.Extra.gatherEqualsBy (\{ country } -> country)
                |> List.map
                    (\( { country }, rest ) ->
                        { country = country
                        , count = List.length rest + 1
                        }
                    )
                |> List.sortBy (\{ count } -> -count)
    in
    Theme.column []
        [ Theme.table []
            { data =
                data
            , columns =
                [ Theme.tableColumnText "Country" .country
                , Theme.tableColumnNumber "Count" .count
                ]
            }
        , data
            |> List.map (\{ country, count } -> ( country, toFloat count ))
            |> Pie.view
            |> Element.html
            |> el [ width <| px 500 ]
        ]


viewCaptchas : Model -> Element Msg
viewCaptchas { invalidCaptchas, inputs } =
    Theme.table []
        { data =
            inputs
                |> List.Extra.gatherEqualsBy (\{ captcha } -> String.toLower captcha)
                |> List.map
                    (\( { captcha }, rest ) ->
                        { captcha = String.toLower captcha
                        , count = List.length rest + 1
                        }
                    )
                |> List.sortBy (\{ count } -> -count)
        , columns =
            [ Theme.tableColumnText "Captcha" .captcha
            , Theme.tableColumnNumber "Count" .count
            , { header = "Is valid"
              , view =
                    \{ captcha } ->
                        let
                            ( label, updater ) =
                                if Set.member captcha invalidCaptchas then
                                    ( "No", Set.remove )

                                else
                                    ( "Yes", Set.insert )
                        in
                        ( []
                        , Theme.button [ centerX ]
                            { onPress =
                                invalidCaptchas
                                    |> updater captcha
                                    |> InvalidCaptchas
                                    |> Just
                            , label = text label
                            }
                        )
              , width = shrink
              }
            ]
        }


card :
    String
    -> Element msg
    -> Element msg
card label child =
    Element.column
        [ width fill
        , alignTop
        ]
        [ el
            [ Font.bold
            , Theme.padding
            , Border.widthEach
                { top = 1
                , left = 1
                , right = 1
                , bottom = 0
                }
            , Border.roundEach
                { topLeft = Theme.rythm
                , topRight = Theme.rythm
                , bottomLeft = 0
                , bottomRight = 0
                }
            , Background.color <| rgb 0.8 0.8 0.8
            ]
            (text label)
        , el
            [ Border.width 1
            , Border.roundEach
                { topLeft = 0
                , topRight = Theme.rythm
                , bottomLeft = Theme.rythm
                , bottomRight = Theme.rythm
                }
            , Theme.padding
            ]
            child
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InvalidCaptchas invalidCaptchas ->
            ( { model | invalidCaptchas = invalidCaptchas }, Cmd.none )

        GotIndexData (Err e) ->
            ( { model | indexData = Failure e }, Cmd.none )

        GotCapitalsData result ->
            ( { model | capitals = RemoteData.fromResult result }, Cmd.none )

        GotIndexData (Ok result) ->
            let
                newModel : Model
                newModel =
                    { model
                        | indexData = Success result
                    }

                countries : List String
                countries =
                    model.inputs
                        |> List.map
                            (.country
                                >> normalizeCountry
                            )
                        |> Set.fromList
                        |> Set.toList

                cmds : List (Cmd Msg)
                cmds =
                    countries
                        |> List.map (loadCountry newModel)
            in
            ( { newModel
                | geoJsonData =
                    countries
                        |> List.map (\country -> ( country, Loading ))
                        |> Dict.fromList
              }
            , Cmd.batch cmds
            )

        GotGeoJson country result ->
            ( { model
                | geoJsonData = Dict.insert country (RemoteData.fromResult result) model.geoJsonData
              }
            , Cmd.none
            )

        ReloadCountry country ->
            ( model, loadCountry model country )


normalizeCountry : String -> String
normalizeCountry =
    String.replace "UK" "United Kingdom"


loadCountry : Model -> Country -> Cmd Msg
loadCountry model country =
    case model.indexData of
        Success result ->
            case Dict.get country result of
                Nothing ->
                    Task.perform
                        (GotGeoJson country)
                        (Task.succeed (Err Nothing))

                Just { threeLetterCode, level } ->
                    List.range 0 (level - 2)
                        |> List.map
                            (\lvl ->
                                Http.Tasks.get
                                    { url = geodataUrl threeLetterCode lvl
                                    , resolver = Http.Tasks.resolveJson geoJsonDecoder
                                    }
                            )
                        |> Task.sequence
                        |> Task.map (List.concat >> List.reverse)
                        |> Task.attempt (GotGeoJson country << Result.mapError Just)

        _ ->
            Cmd.none


geoJsonDecoder : Decoder (List Location)
geoJsonDecoder =
    GeoJson.decoder
        |> Json.Decode.andThen
            (\( geoJsonObject, _ ) ->
                case geoJsonObject of
                    Geometry _ ->
                        Json.Decode.fail "Unexpected geometry"

                    Feature _ ->
                        Json.Decode.fail "Unexpected feature"

                    FeatureCollection objects ->
                        case
                            objects
                                |> Result.Extra.combineMap
                                    (\object ->
                                        Result.map2
                                            (\( name, varName ) geometry ->
                                                { name = name
                                                , alternativeNames =
                                                    if varName == "NA" then
                                                        []

                                                    else
                                                        String.split "|" varName
                                                , center = getCentroid geometry
                                                , geometry = geometry
                                                }
                                            )
                                            (Json.Decode.decodeValue nameDecoder
                                                object.properties
                                                |> Result.mapError Json.Decode.errorToString
                                            )
                                            (object.geometry
                                                |> Result.fromMaybe "Missing geometry"
                                            )
                                    )
                        of
                            Ok result ->
                                Json.Decode.succeed result

                            Err e ->
                                Json.Decode.fail e
            )


getCentroid : Geometry -> Position
getCentroid geometry =
    let
        getMedian : List Position -> Position
        getMedian list =
            let
                len : Int
                len =
                    List.length list

                medianLength : Int
                medianLength =
                    -- 1 if odd, 2 if even
                    1 + modBy 2 len

                do : (Position -> Float) -> Float
                do f =
                    list
                        |> List.map f
                        |> List.sort
                        |> List.drop (len // 2)
                        |> List.take medianLength
                        |> List.sum
                        |> (\s -> s / toFloat medianLength)
            in
            ( do <| \( lat, _, _ ) -> lat
            , do <| \( _, lon, _ ) -> lon
            , do <| \( _, _, alt ) -> alt
            )
    in
    case geometry of
        Point p ->
            p

        MultiPoint positions ->
            getMedian positions

        LineString positions ->
            getMedian positions

        MultiLineString positions ->
            getMedian (List.concat positions)

        Polygon positions ->
            getMedian (List.concat positions)

        MultiPolygon positions ->
            getMedian (List.concat <| List.concat positions)

        GeometryCollection geometries ->
            getMedian (List.map getCentroid geometries)


nameDecoder : Decoder ( String, String )
nameDecoder =
    let
        decoderAtLevel : Int -> Decoder ( String, String )
        decoderAtLevel level =
            let
                suffix : String
                suffix =
                    String.fromInt level
            in
            Json.Decode.field ("NAME_" ++ suffix) Json.Decode.string
                |> Json.Decode.andThen
                    (\name ->
                        Json.Decode.oneOf
                            [ decoderAtLevel (level + 1)
                            , Json.Decode.oneOf
                                [ Json.Decode.field ("VARNAME_" ++ suffix) Json.Decode.string
                                , Json.Decode.succeed ""
                                ]
                                |> Json.Decode.map
                                    (\varName ->
                                        ( name, varName )
                                    )
                            ]
                    )
    in
    Json.Decode.oneOf
        [ decoderAtLevel 1
        , Json.Decode.map (\country -> ( country, "" )) <|
            Json.Decode.field "COUNTRY" Json.Decode.string
        ]
