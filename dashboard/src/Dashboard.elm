module Dashboard exposing (init, update, view)

import Dict exposing (Dict)
import Element exposing (Element, alignTop, centerX, el, fill, paragraph, px, rgb, shrink, text, width, wrappedRow)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import File.Download
import GeoJson exposing (GeoJsonObject(..), Geometry(..), Position)
import Http
import Http.Tasks
import Json.Decode exposing (Decoder)
import List.Extra
import Map
import Pie
import RemoteData exposing (RemoteData(..))
import Result.Extra
import Round
import Set
import Task
import Theme
import Theme.Dashboard as Theme
import Types exposing (Country, Input, Location, Model, Msg(..))


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
        , map : Element Msg
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
                        case Types.findPosition model input of
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
    , map = Map.view model locations
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
                                >> Types.normalizeCountry
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

        Download { name, content } ->
            ( model, File.Download.string name "image/svg+xml" content )


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
