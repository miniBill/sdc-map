module Main exposing (main)

import Base64
import Browser
import Dashboard
import Element exposing (Element, el, paragraph, text)
import Element.Input as Input
import Flate
import Serialize
import Set exposing (Set)
import Theme
import Types exposing (Input)


type Model
    = WaitingInput { input : String, error : Maybe String }
    | Ready
        { inputs : List Input
        , invalidCaptchas : Set String
        }


type Msg
    = Input String
    | Load
    | InvalidCaptchas (Set String)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = \model -> Element.layout [] (view model)
        , update = update
        }


init : Model
init =
    WaitingInput { input = "", error = Nothing }


view : Model -> Element Msg
view model =
    case model of
        WaitingInput { input, error } ->
            Theme.column [ Theme.padding ]
                [ Input.text []
                    { label = Input.labelHidden "Server data"
                    , text = input
                    , onChange = Input
                    , placeholder = Nothing
                    }
                , Theme.button []
                    { onPress = Just Load
                    , label = text "Load"
                    }
                , case error of
                    Nothing ->
                        Element.none

                    Just e ->
                        paragraph
                            [ Theme.padding ]
                            [ text <| "Error: " ++ e ]
                ]

        Ready { invalidCaptchas, inputs } ->
            el [ Theme.padding ] <|
                Element.map InvalidCaptchas <|
                    Dashboard.view invalidCaptchas inputs


update : Msg -> Model -> Model
update msg model =
    case ( msg, model ) of
        ( Input input, WaitingInput _ ) ->
            WaitingInput { input = input, error = Nothing }

        ( Load, WaitingInput { input } ) ->
            case
                input
                    |> Base64.toBytes
                    |> Result.fromMaybe "Could not read base64 data"
                    |> Result.andThen
                        (\bytes ->
                            bytes
                                |> Flate.inflate
                                |> Result.fromMaybe "Could not inflate data"
                        )
                    |> Result.andThen
                        (\bytes ->
                            bytes
                                |> Serialize.decodeFromBytes
                                    (Serialize.list Types.inputCodec)
                                |> Result.mapError Debug.toString
                        )
            of
                Ok inputs ->
                    Ready
                        { inputs = inputs
                        , invalidCaptchas = Set.empty
                        }

                Err e ->
                    WaitingInput
                        { input = input
                        , error = Just e
                        }

        ( InvalidCaptchas _, WaitingInput _ ) ->
            model

        ( InvalidCaptchas invalidCaptchas, Ready ready ) ->
            Ready { ready | invalidCaptchas = invalidCaptchas }

        ( Input _, Ready _ ) ->
            model

        ( Load, Ready _ ) ->
            model
