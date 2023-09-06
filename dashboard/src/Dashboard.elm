module Dashboard exposing (Model, Msg, init, update, view)

import Dict exposing (Dict)
import Element exposing (Column, Element, alignTop, centerY, el, fill, px, row, shrink, text, width)
import Element.Border as Border
import Element.Font as Font
import GeoJson exposing (GeoJson)
import List.Extra
import Pie
import Set exposing (Set)
import Theme
import Types exposing (Input)


type Msg
    = InvalidCaptchas (Set String)


type alias Model =
    { invalidCaptchas : Set String
    , inputs : List Input
    , loadedGeoJsonData : Dict Country GeoJson
    }


type alias Country =
    String


view : Model -> Element Msg
view model =
    row [ Theme.spacing, width fill ]
        [ Theme.column
            [ width fill
            , alignTop
            ]
            [ viewOnMap model
            , viewByCountry model
            ]
        , viewCaptchas model
        ]


validInputs : Model -> List Input
validInputs model =
    model.inputs
        |> List.filter (\{ captcha } -> not (Set.member captcha model.invalidCaptchas))


viewOnMap : Model -> Element Msg
viewOnMap model =
    card "On map"
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
        , pie = Nothing
        }


viewByCountry : Model -> Element Msg
viewByCountry model =
    card "Statistics by country"
        { data =
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
        , columns =
            [ tableColumn "Country" .country
            , tableColumn "Count" <| \{ count } -> String.fromInt count
            ]
        , pie = Just <| \{ country, count } -> ( country, toFloat count )
        }


tableColumn : String -> (element -> String) -> Column element msg
tableColumn header toCell =
    { header = el [ Font.bold ] <| text header
    , view = \row -> el [ centerY ] <| text (toCell row)
    , width = shrink
    }


viewCaptchas : Model -> Element Msg
viewCaptchas { invalidCaptchas, inputs } =
    card "Captchas"
        { data =
            inputs
                |> List.Extra.gatherEqualsBy (\{ captcha } -> captcha)
                |> List.map
                    (\( { captcha }, rest ) ->
                        { captcha = captcha
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
        , pie = Nothing
        }


card :
    String
    ->
        { data : List record
        , columns : List (Column record msg)
        , pie : Maybe (record -> ( String, Float ))
        }
    -> Element msg
card label { data, columns, pie } =
    Theme.column
        [ Border.width 1
        , Theme.padding
        , width fill
        , alignTop
        ]
        [ el [ Font.bold ] <| text label
        , Element.table [ Theme.spacing ]
            { data = data
            , columns = columns
            }
        , case pie of
            Nothing ->
                Element.none

            Just f ->
                data
                    |> List.map f
                    |> List.sortBy (\( _, count ) -> count)
                    |> Pie.view
                    |> Element.html
                    |> el [ width <| px 500 ]
        ]


init : List Input -> ( Model, Cmd msg )
init inputs =
    ( { inputs = inputs
      , invalidCaptchas = Set.empty
      , loadedGeoJsonData = Dict.empty
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        InvalidCaptchas invalidCaptchas ->
            ( { model | invalidCaptchas = invalidCaptchas }, Cmd.none )
