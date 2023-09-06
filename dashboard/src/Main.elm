module Main exposing (main)

import Base64
import Browser
import Dashboard
import Element exposing (Element, el, paragraph, text)
import Element.Font as Font
import Element.Input as Input
import Flate
import Html.Attributes
import Serialize
import Theme
import Types


type Model
    = WaitingInput { input : String, error : Maybe String }
    | Dashboard Dashboard.Model


type Msg
    = Input String
    | Load
    | DashboardMsg Dashboard.Msg


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = \model -> Element.layout [] (view model)
        , update = update
        , subscriptions = \_ -> Sub.none
        }


init : flags -> ( Model, Cmd msg )
init _ =
    ( WaitingInput { input = "", error = Nothing }
    , Cmd.none
    )


view : Model -> Element Msg
view model =
    case model of
        WaitingInput { input, error } ->
            Theme.column [ Theme.padding ]
                [ Input.text [ Element.htmlAttribute <| Html.Attributes.autofocus True ]
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( Input input, WaitingInput _ ) ->
            ( WaitingInput { input = input, error = Nothing }, Cmd.none )

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
                    let
                        ( submodel, subcmd ) =
                            Dashboard.init inputs
                    in
                    ( Dashboard submodel, Cmd.map DashboardMsg subcmd )

                Err e ->
                    ( WaitingInput
                        { input = input
                        , error = Just e
                        }
                    , Cmd.none
                    )

        ( DashboardMsg _, WaitingInput _ ) ->
            ( model, Cmd.none )

        ( DashboardMsg submsg, Dashboard submodel ) ->
            let
                ( newSubmodel, newSubcmd ) =
                    Dashboard.update submsg submodel
            in
            ( Dashboard newSubmodel, Cmd.map DashboardMsg newSubcmd )

        ( Input _, Dashboard _ ) ->
            ( model, Cmd.none )

        ( Load, Dashboard _ ) ->
            ( model, Cmd.none )


errorToString : Serialize.Error Never -> String
errorToString error =
    case error of
        Serialize.CustomError ever ->
            never ever

        Serialize.DataCorrupted ->
            "Data corrupted"

        Serialize.SerializerOutOfDate ->
            "Serializer out of date"
