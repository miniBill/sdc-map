module Main exposing (main)

import Base64
import Browser
import Dashboard
import Element exposing (Element, el, paragraph, text)
import Element.Input as Input
import Flate
import Serialize
import Theme
import Types exposing (Input)


type Model
    = WaitingInput { input : String, error : Maybe String }
    | Dashboard Dashboard.Model


type Msg
    = Input String
    | Load
    | DashboardMsg Dashboard.Model


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

        Dashboard dashboardModel ->
            Dashboard.view dashboardModel
                |> Element.map DashboardMsg
                |> el [ Theme.padding ]


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
                                |> Result.mapError errorToString
                        )
            of
                Ok inputs ->
                    Dashboard (Dashboard.init inputs)

                Err e ->
                    WaitingInput
                        { input = input
                        , error = Just e
                        }

        ( DashboardMsg _, WaitingInput _ ) ->
            model

        ( DashboardMsg submsg, Dashboard submodel ) ->
            Dashboard (Dashboard.update submsg submodel)

        ( Input _, Dashboard _ ) ->
            model

        ( Load, Dashboard _ ) ->
            model


errorToString : Serialize.Error Never -> String
errorToString error =
    case error of
        Serialize.CustomError ever ->
            never ever

        Serialize.DataCorrupted ->
            "Data corrupted"

        Serialize.SerializerOutOfDate ->
            "Serializer out of date"
