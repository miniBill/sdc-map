module Dashboard exposing (Model, Msg, init, update, view)

import Dict exposing (Dict)
import Element exposing (Column, Element, alignTop, centerY, el, fill, px, row, shrink, text, width)
import Element.Border as Border
import Element.Font as Font
import GeoJson exposing (GeoJson)
import Http
import Json.Decode exposing (Decoder)
import List.Extra
import Pie
import RemoteData exposing (WebData)
import Set exposing (Set)
import Theme
import Types exposing (Input)


type Msg
    = InvalidCaptchas (Set String)
    | GotIndexData (Result Http.Error (Dict String { threeLetterCode : String, level : Int }))


type alias Model =
    { invalidCaptchas : Set String
    , inputs : List Input
    , indexData : WebData (Dict String { threeLetterCode : String, level : Int })
    , loadedGeoJsonData : Dict Country GeoJson
    }


init : List Input -> ( Model, Cmd Msg )
init inputs =
    ( { inputs = inputs
      , invalidCaptchas = Set.empty
      , loadedGeoJsonData = Dict.empty
      , indexData = RemoteData.Loading
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
    Theme.column []
        [ row
            [ Theme.spacing, width fill ]
            [ viewByCountry model
            , viewCaptchas model
            ]
        , viewOnMap model
        ]


validInputs : Model -> List Input
validInputs model =
    model.inputs
        |> List.filter (\{ captcha } -> not (Set.member (String.toLower captcha) model.invalidCaptchas))


viewOnMap : Model -> Element Msg
viewOnMap model =
    Theme.column []
        [ card "On map"
            [ Element.table [ Theme.spacing ]
                { data =
                    validInputs model
                        |> List.filter
                            (\{ nameOnMap } ->
                                (nameOnMap == Just True)
                            )
                , columns =
                    [ tableColumn "Name" .name
                    , tableColumn "Country" .country
                    , tableColumn "Location" .location
                    ]
                }
            ]
        ]


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
    card "Statistics by country"
        [ Element.table [ Theme.spacing ]
            { data =
                data
            , columns =
                [ tableColumn "Country" .country
                , tableColumn "Count" <| \{ count } -> String.fromInt count
                ]
            }
        , data
            |> List.map (\{ country, count } -> ( country, toFloat count ))
            |> Pie.view
            |> Element.html
            |> el [ width <| px 500 ]
        ]


tableColumn : String -> (element -> String) -> Column element msg
tableColumn header toCell =
    { header = el [ Font.bold ] <| text header
    , view = \row -> el [ centerY ] <| text (toCell row)
    , width = shrink
    }


viewCaptchas : Model -> Element Msg
viewCaptchas { invalidCaptchas, inputs } =
    card "Captchas"
        [ Element.table [ Theme.spacing ]
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
                [ tableColumn "Captcha" .captcha
                , tableColumn "Count" <| \{ count } -> String.fromInt count
                , { header = el [ Font.bold ] <| text "Is valid"
                  , view =
                        \{ captcha } ->
                            let
                                ( label, updater ) =
                                    if Set.member captcha invalidCaptchas then
                                        ( "No", Set.remove )

                                    else
                                        ( "Yes", Set.insert )
                            in
                            Theme.button []
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
        ]


card :
    String
    -> List (Element msg)
    -> Element msg
card label children =
    Theme.column
        [ Border.width 1
        , Theme.padding
        , width fill
        , alignTop
        ]
        (el [ Font.bold ] (text label)
            :: children
        )


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        InvalidCaptchas invalidCaptchas ->
            ( { model | invalidCaptchas = invalidCaptchas }, Cmd.none )

        GotIndexData result ->
            ( { model | indexData = RemoteData.fromResult result }, Cmd.none )
