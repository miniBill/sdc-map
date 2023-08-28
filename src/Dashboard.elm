module Dashboard exposing (view)

import Element exposing (Column, Element, alignTop, centerY, el, fill, shrink, text, width)
import Element.Border as Border
import Element.Font as Font
import List.Extra
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
            [ Theme.column
                [ Border.width 1
                , Theme.padding
                , width fill
                ]
                [ el [ Font.bold ] <| text "On map"
                , Element.table [ Theme.spacing ]
                    { data =
                        validInputs
                            |> List.filter (\{ nameOnMap } -> nameOnMap == Just True)
                    , columns =
                        [ column "Name" .name
                        , column "Country" .country
                        , column "Location" .location
                        ]
                    }
                ]
            , Theme.column
                [ Border.width 1
                , Theme.padding
                , width fill
                ]
                [ el [ Font.bold ] <| text "Statistics by country"
                , Element.table [ Theme.spacing ]
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
                    }
                ]
            ]
        , Theme.column
            [ Border.width 1
            , Theme.padding
            , alignTop
            ]
            [ el [ Font.bold ] <| text "Captchas"
            , Element.table [ Theme.spacing ]
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
                            \row ->
                                let
                                    invalid : Bool
                                    invalid =
                                        Set.member row.captcha invalidCaptchas
                                in
                                Theme.button []
                                    { onPress = Just <| CaptchaIsValid row.captcha invalid
                                    , label =
                                        if invalid then
                                            text "No"

                                        else
                                            text "Yes"
                                    }
                      , width = shrink
                      }
                    ]
                }
            ]
        ]
