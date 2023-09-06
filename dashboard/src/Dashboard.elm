module Dashboard exposing (Model, Msg, init, update, view)

import Dict exposing (Dict)
import Element exposing (Attribute, Column, Element, alignRight, alignTop, centerX, centerY, el, fill, paragraph, px, rgb, row, shrink, text, width, wrappedRow)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import GeoJson exposing (GeoJsonObject(..), Geometry(..), Position)
import Http
import Http.Tasks
import Json.Decode exposing (Decoder)
import List.Extra
import Pie
import RemoteData exposing (RemoteData(..), WebData)
import Result.Extra
import Set exposing (Set)
import Task
import Theme
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
        { onMap, geoData, locations } =
            viewOnMap model
    in
    [ card "Statistics by country" <| viewByCountry model
    , Theme.column [ alignTop ]
        [ card "Captchas" <| viewCaptchas model
        , card "GeoData" geoData
        ]
    , card "On map" onMap
    , card "Locations" locations
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
        , geoData : Element Msg
        , locations : Element msg
        }
viewOnMap model =
    let
        filteredInputs =
            validInputs model
                |> List.filter
                    (\{ nameOnMap } ->
                        (nameOnMap == Just True)
                    )

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
                                            List.range 1 (level - 2)
                                                |> List.map
                                                    (\lvl ->
                                                        "geodata/gadm41_"
                                                            ++ threeLetterCode
                                                            ++ "_"
                                                            ++ String.fromInt lvl
                                                            ++ ".json"
                                                    )
                                        )

                            _ ->
                                Nothing
                    )
                |> List.concat
    in
    { onMap =
        table [ alignTop ]
            { data = filteredInputs
            , columns =
                [ tableColumnText "Name" .name
                , tableColumnText "Country" .country
                , tableColumnText "Location" .location
                ]
            }
    , geoData =
        Theme.column [ alignTop ]
            [ case model.indexData of
                Loading ->
                    text "Loading index data..."

                NotAsked ->
                    text "Something went wrong with the index data."

                Failure e ->
                    text <| Debug.toString e

                Success _ ->
                    text "Loaded index data."
            , table []
                { data = Dict.toList model.geoJsonData
                , columns =
                    [ Just <| tableColumnText "Country" Tuple.first
                    , Just <|
                        tableColumnText "Status" <|
                            \( _, geoJson ) ->
                                case geoJson of
                                    Failure (Just e) ->
                                        httpErrorToString e

                                    Failure Nothing ->
                                        "Not found in index"

                                    NotAsked ->
                                        "Not asked???"

                                    Loading ->
                                        "Loading..."

                                    Success _ ->
                                        "Loaded"
                    , if List.isEmpty needed then
                        Nothing

                      else
                        { header = text "Commands"
                        , width = shrink
                        , view =
                            \( country, geoJson ) ->
                                case geoJson of
                                    Failure (Just _) ->
                                        Theme.button []
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

                                    _ ->
                                        Element.none
                        }
                            |> Just
                    ]
                        |> List.filterMap identity
                }
            , if List.isEmpty needed then
                Element.none

              else
                paragraph [] [ text <| String.join " " <| "make -j" :: needed ]
            ]
    , locations =
        table []
            { data =
                filteredInputs
                    |> List.Extra.gatherEqualsBy (\{ country, location } -> ( country, location ))
                    |> List.map Tuple.first
                    |> List.sortBy (\{ country, location } -> ( country, location ))
            , columns =
                [ tableColumnText "Country" .country
                , tableColumnText "Location" .location
                , tableColumnText "Found?" (\input -> Debug.toString <| findPosition model input)
                ]
            }
    }


findPosition : Model -> Input -> Result String Position
findPosition model { country, location } =
    if String.isEmpty location then
        case model.capitals of
            Loading ->
                Err "Loading"

            NotAsked ->
                Err "Not asked?"

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

            Just NotAsked ->
                Err "Not asked?"

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
        [ table []
            { data =
                data
            , columns =
                [ tableColumnText "Country" .country
                , tableColumnNumber "Count" .count
                ]
            }
        , data
            |> List.map (\{ country, count } -> ( country, toFloat count ))
            |> Pie.view
            |> Element.html
            |> el [ width <| px 500 ]
        ]


tableColumnNumber : String -> (element -> Int) -> Column element msg
tableColumnNumber headerLabel toCell =
    { header = text headerLabel
    , view = \row -> el [ centerY, alignRight ] <| text <| String.fromInt <| toCell row
    , width = shrink
    }


tableColumnText : String -> (element -> String) -> Column element msg
tableColumnText headerLabel toCell =
    { header = text headerLabel
    , view = \row -> el [ centerY ] <| text <| toCell row
    , width = shrink
    }


viewCaptchas : Model -> Element Msg
viewCaptchas { invalidCaptchas, inputs } =
    table []
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
            [ tableColumnText "Captcha" .captcha
            , tableColumnNumber "Count" .count
            , { header = text "Is valid"
              , view =
                    \{ captcha } ->
                        let
                            ( label, updater ) =
                                if Set.member captcha invalidCaptchas then
                                    ( "No", Set.remove )

                                else
                                    ( "Yes", Set.insert )
                        in
                        Theme.button [ centerX ]
                            { onPress =
                                invalidCaptchas
                                    |> updater captcha
                                    |> InvalidCaptchas
                                    |> Just
                            , label = text label
                            }
              , width = shrink
              }
            ]
        }


table : List (Attribute msg) -> { data : List record, columns : List (Column record msg) } -> Element msg
table attrs config =
    Element.indexedTable attrs
        { data = config.data
        , columns =
            config.columns
                |> List.indexedMap
                    (\col column ->
                        { header = header col column.header
                        , view = \row -> cell row col column.view
                        , width = column.width
                        }
                    )
        }


cell : Int -> Int -> (record -> Element msg) -> record -> Element msg
cell row col cellView record =
    let
        leftPadding =
            if col == 0 then
                0

            else
                Theme.rythm

        topPadding =
            if row == 0 then
                Theme.rythm // 2

            else
                Theme.rythm
    in
    el
        [ Element.paddingEach
            { left = leftPadding
            , right = 0
            , top = topPadding
            , bottom = 0
            }
        , centerY
        ]
        (cellView record)


header : Int -> Element msg -> Element msg
header col child =
    el
        [ Font.bold
        , Border.widthEach
            { top = 0
            , left = 0
            , right = 0
            , bottom = 1
            }
        , if col == 0 then
            Element.paddingEach
                { left = 0
                , right = 0
                , top = 0
                , bottom = Theme.rythm // 2
                }

          else
            Element.paddingEach
                { left = Theme.rythm
                , right = 0
                , top = 0
                , bottom = Theme.rythm // 2
                }
        ]
        child


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
                    List.range 1 (level - 2)
                        |> List.map
                            (\lvl ->
                                Http.Tasks.get
                                    { url =
                                        "/geodata/gadm41_"
                                            ++ threeLetterCode
                                            ++ "_"
                                            ++ String.fromInt lvl
                                            ++ ".json"
                                    , resolver = Http.Tasks.resolveJson geoJsonDecoder
                                    }
                            )
                        |> Task.sequence
                        |> Task.map List.concat
                        |> Task.attempt (GotGeoJson country << Result.mapError Just)

        _ ->
            Cmd.none


geoJsonDecoder : Decoder (List Location)
geoJsonDecoder =
    GeoJson.decoder
        |> Json.Decode.andThen
            (\( geoJsonObject, _ ) ->
                -- "NAME_1": "Kärnten",
                -- "VARNAME_1": "Carinthia|Caríntia|Carintia",
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
        decoderAtLevel level =
            let
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
    decoderAtLevel 1
