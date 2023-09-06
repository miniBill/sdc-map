module Dashboard exposing (Model, Msg, init, update, view)

import Dict exposing (Dict)
import Element exposing (Attribute, Column, Element, alignRight, alignTop, centerX, centerY, el, fill, paragraph, px, rgb, row, shrink, text, width, wrappedRow)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import GeoJson exposing (GeoJson, GeoJsonObject(..))
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
    | GotGeoJson Country (Result (Maybe Http.Error) (List GeoJson))
    | ReloadCountry Country


type alias Model =
    { invalidCaptchas : Set String
    , inputs : List Input
    , indexData : WebData (Dict String { threeLetterCode : String, level : Int })
    , geoJsonData : Dict Country (RemoteData (Maybe Http.Error) (List GeoJson))
    }


init : List Input -> ( Model, Cmd Msg )
init inputs =
    ( { inputs = inputs
      , invalidCaptchas = Set.empty
      , geoJsonData = Dict.empty
      , indexData = Loading
      }
    , Http.get
        { url = "/geodata-index.json"
        , expect = Http.expectJson GotIndexData indexDataDecoder
        }
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
                , tableColumnText "Found?" (found model)
                ]
            }
    }


found : Model -> Input -> String
found model { country, location } =
    case Dict.get (normalizeCountry country) model.geoJsonData of
        Nothing ->
            "Missing (data)"

        Just Loading ->
            "Loading"

        Just NotAsked ->
            "Not asked?"

        Just (Failure Nothing) ->
            "Missing (index)"

        Just (Failure _) ->
            "Failure"

        Just (Success geoJsons) ->
            case geoJsons of
                [] ->
                    "No jsons loaded"

                _ ->
                    geoJsons
                        |> Result.Extra.combineMap
                            (\( geoJson, _ ) ->
                                case geoJson of
                                    Geometry _ ->
                                        Err "Unexpected geometry"

                                    Feature _ ->
                                        Err "Unexpected feature"

                                    FeatureCollection objects ->
                                        Ok objects
                            )
                        |> Result.map List.concat
                        |> Result.map (\objects -> "TODO (" ++ String.fromInt (List.length objects) ++ ")")
                        |> Result.Extra.merge


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
                                    , resolver = Http.Tasks.resolveJson GeoJson.decoder
                                    }
                            )
                        |> Task.sequence
                        |> Task.attempt (GotGeoJson country << Result.mapError Just)

        _ ->
            Cmd.none
