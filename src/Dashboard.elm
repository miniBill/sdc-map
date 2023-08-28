module Dashboard exposing (view)

import Element exposing (Column, Element, alignTop, centerY, el, fill, px, shrink, text, width)
import Element.Border as Border
import Element.Font as Font
import List.Extra
import Pie
import Set exposing (Set)
import Theme
import Types exposing (FrontendMsg(..), Input)


view : Set String -> List Input -> Element FrontendMsg
view invalidCaptchas inputs =
    let
        validInputs : List Input
        validInputs =
            inputs
                |> List.filter (\{ captcha } -> not (Set.member captcha invalidCaptchas))

        column : String -> (element -> String) -> Column element msg
        column header toCell =
            { header = el [ Font.bold ] <| text header
            , view = \row -> el [ centerY ] <| text (toCell row)
            , width = shrink
            }
    in
    Theme.row [ width fill ]
        [ Theme.column
            [ width fill
            , alignTop
            ]
            [ card "On map"
                { data =
                    validInputs
                        |> List.filter (\{ nameOnMap } -> nameOnMap == Just True)
                , columns =
                    [ column "Name" .name
                    , column "Country" .country
                    , column "Location" .location
                    ]
                , pie = Nothing
                }
            , card "Statistics by country"
                { data =
                    validInputs
                        |> List.Extra.gatherEqualsBy (\{ country } -> country)
                        |> List.map
                            (\( { country }, rest ) ->
                                { country = country
                                , count = List.length rest + 1
                                }
                            )
                        |> List.sortBy (\{ count } -> -count)
                , columns =
                    [ column "Country" .country
                    , column "Count" <| \{ count } -> String.fromInt count
                    ]
                , pie = Just <| \{ country, count } -> ( country, toFloat count )
                }
            ]
        , card "Captchas"
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
                [ column "Captcha" .captcha
                , column "Count" <| \{ count } -> String.fromInt count
                , { header = el [ Font.bold ] <| text "Is valid"
                  , view =
                        \{ captcha } ->
                            let
                                invalid : Bool
                                invalid =
                                    Set.member captcha invalidCaptchas
                            in
                            Theme.button []
                                { onPress = Just <| CaptchaIsValid captcha invalid
                                , label =
                                    if invalid then
                                        text "No"

                                    else
                                        text "Yes"
                                }
                  , width = shrink
                  }
                ]
            , pie = Nothing
            }
        ]


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
