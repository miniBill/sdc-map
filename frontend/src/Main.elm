port module Main exposing (Flags, Model, Msg, main)

import Browser
import Dict
import Html
import Html.Attributes
import Http
import Json.Encode exposing (Value)
import Subdivisions
import Theme
import Ui exposing (Element, centerX, el, fill, height, rgb, text, width)
import Ui.Events
import Ui.Font as Font
import Ui.Input as Input
import Ui.Prose exposing (paragraph)
import Ui.Table as Table


port encrypt : Value -> Cmd msg


port encrypted : (String -> msg) -> Sub msg


type alias Input =
    { name : String
    , country : String
    , location : String
    , nameOnMap : Bool
    , originallyFrom : String
    , id : String
    , captcha : String
    }


type alias Flags =
    {}


type EncryptedString
    = EncryptedString String


type Model
    = Filling Input (Maybe Error)
    | Encrypting Input
    | Submitting Input
    | Submitted Input


type alias Error =
    String


type Msg
    = Name String
    | Country String
    | Location String
    | NameOnMap Bool
    | OriginallyFrom String
    | Id String
    | Captcha String
    | Edit
    | SubmitResult (Result Http.Error String)
    | Submit
    | Encrypted EncryptedString


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view =
            \flagsModel ->
                Ui.layout
                    [ width fill
                    , height fill
                    , Theme.padding
                    ]
                    (view flagsModel)
        , update = update
        , subscriptions = subscriptions
        }


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( Filling
        { name = ""
        , country = ""
        , location = ""
        , nameOnMap = True
        , originallyFrom = ""
        , captcha = ""
        , id = ""
        }
        Nothing
    , Cmd.none
    )


view : Model -> Element Msg
view model =
    case model of
        Filling input maybeError ->
            Theme.column
                [ Theme.padding
                , centerX
                ]
                [ viewInput input
                , case maybeError of
                    Just error ->
                        viewError error

                    Nothing ->
                        Ui.none
                , if isValid input then
                    Ui.el
                        [ Ui.Events.onClick Submit
                        , Theme.padding
                        , Ui.border 1
                        ]
                        (text "Submit")

                  else
                    Ui.none
                ]

        Encrypting input ->
            Theme.column
                [ Theme.padding
                , centerX
                ]
                [ viewInputReadonly input
                , el [] <| text " "
                , text "Encrypting..."
                ]

        Submitting input ->
            Theme.column
                [ Theme.padding
                , centerX
                ]
                [ viewInputReadonly input
                , el [] <| text " "
                , text "Submitting..."
                ]

        Submitted input ->
            Theme.column
                [ Theme.padding
                , centerX
                ]
                [ viewInputReadonly input
                , paragraph []
                    [ text "Thank you for your submission 😊" ]
                , paragraph []
                    [ text "If you want to change anything, just "
                    , Ui.el [ Ui.Events.onClick Edit ]
                        (el [ Font.underline ] <|
                            text "edit and submit again."
                        )
                    ]
                ]


isValid : Input -> Bool
isValid { name, country, captcha } =
    (String.isEmpty name
        || String.isEmpty country
        || String.isEmpty captcha
    )
        |> not


viewInputReadonly : Input -> Element Msg
viewInputReadonly input =
    let
        inputRow : String -> String -> ( Table.Cell msg, Table.Cell msg )
        inputRow label value =
            ( Table.cell [ Font.alignRight ] <| text <| label ++ ": "
            , Table.cell [ Font.weight Font.bold ] <| text value
            )

        config : Table.Config () ( Table.Cell msg, Table.Cell msg ) msg
        config =
            Table.columns
                [ Table.column
                    { header = Table.cell [] Ui.none
                    , view = Tuple.first
                    }
                , Table.column
                    { header = Table.cell [] Ui.none
                    , view = Tuple.second
                    }
                ]
    in
    Table.view [ Ui.width Ui.shrink, Theme.spacing ]
        config
        [ inputRow "Name" input.name
        , inputRow "Country" input.country
        , inputRow "Location" input.location
        , inputRow "Name on map"
            (if input.nameOnMap then
                "Yes"

             else
                "No"
            )
        , inputRow "Originally from" input.originallyFrom
        , inputRow "Id" input.id
        ]


viewError : String -> Element Msg
viewError message =
    ("Error: " ++ message)
        |> text
        |> el [ Ui.background <| Ui.rgb 255 204 204 ]


viewInput : Input -> Element Msg
viewInput input =
    let
        inputRow : String -> String -> String -> Bool -> (String -> Msg) -> String -> List String -> List (Element Msg)
        inputRow autocomplete label description mandatory toMsg value autocompleteList =
            [ Input.text
                ([ Html.Attributes.autocomplete True
                 , Html.Attributes.name autocomplete
                 , Html.Attributes.id autocomplete
                 , if List.isEmpty autocompleteList then
                    Html.Attributes.classList []

                   else
                    Html.Attributes.list (autocomplete ++ "-list")
                 ]
                    |> List.map Ui.htmlAttribute
                )
                { label = toLabel label description mandatory
                , text = value
                , onChange = toMsg
                , placeholder = Nothing
                }
            , autocompleteList
                |> List.map
                    (\opt ->
                        Html.option
                            [ Html.Attributes.value opt ]
                            []
                    )
                |> Html.datalist
                    [ Html.Attributes.id (autocomplete ++ "-list") ]
                |> Ui.html
            ]

        checkboxRow : String -> String -> Bool -> (Bool -> Msg) -> Bool -> Element Msg
        checkboxRow label description mandatory toMsg value =
            Input.checkbox [ Ui.width Ui.shrink ]
                { label = toLabel label description mandatory
                , checked = value
                , onChange = toMsg
                , icon = Nothing
                }

        toLabel : String -> String -> Bool -> Input.Label Msg
        toLabel label description mandatory =
            Input.labelAbove [ Ui.width Ui.shrink ] <|
                paragraph []
                    [ text label
                    , if mandatory then
                        el [ Font.color <| Ui.rgb 255 0 0 ] <| text "*"

                      else
                        Ui.none
                    , text " "
                    , el
                        [ Font.size 14
                        , Font.color <| Ui.rgb 153 153 153
                        ]
                        (text description)
                    ]

        locations : List String
        locations =
            Dict.get input.country Subdivisions.subdivisions
                |> Maybe.withDefault []
                |> List.sort
    in
    [ inputRow "name" "Name" "" True Name input.name []
    , inputRow "country" "Country" "" True Country input.country (Dict.keys Subdivisions.subdivisions)
    , inputRow "location" "Location" "Where are you from? Pick the name of the region, or a big city near you. DON'T provide your address." False Location input.location locations
    , [ checkboxRow "Show name on map" "Should your name be shown on the map, or just used for statistics?" True NameOnMap input.nameOnMap ]
    , inputRow "originally" "Originally from" "Where are you originally from?" False OriginallyFrom input.originallyFrom []
    , inputRow "contact" "Contact" "Some contact (email/twitter/discord/...) you can use to prove your identity if you want to remove your information." False Id input.id []
    , inputRow "captcha" "Anti-bot" "What is the title of her first album?" True Captcha input.captcha []
    ]
        |> List.intersperse [ text " " ]
        |> List.concat
        |> Theme.column []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( Id id, Filling input maybeError ) ->
            ( Filling { input | id = id } maybeError, Cmd.none )

        ( Name name, Filling input maybeError ) ->
            ( Filling { input | name = name } maybeError, Cmd.none )

        ( Country country, Filling input maybeError ) ->
            ( Filling { input | country = country } maybeError, Cmd.none )

        ( Location location, Filling input maybeError ) ->
            ( Filling { input | location = location } maybeError, Cmd.none )

        ( NameOnMap nameOnMap, Filling input maybeError ) ->
            ( Filling { input | nameOnMap = nameOnMap } maybeError, Cmd.none )

        ( OriginallyFrom originallyFrom, Filling input maybeError ) ->
            ( Filling { input | originallyFrom = originallyFrom } maybeError, Cmd.none )

        ( Captcha captcha, Filling input maybeError ) ->
            ( Filling { input | captcha = captcha } maybeError, Cmd.none )

        ( Submit, Filling input _ ) ->
            ( Encrypting input
            , encrypt <| encodeInput input
            )

        ( Encrypted encryptedString, Encrypting input ) ->
            ( Submitting input
            , Http.post
                { url = "/submit"
                , expect = Http.expectString SubmitResult
                , body =
                    { input = input
                    , encrypted = encryptedString
                    }
                        |> encodeForServer
                        |> Http.jsonBody
                }
            )

        ( SubmitResult (Ok _), Submitting input ) ->
            ( Submitted input, Cmd.none )

        ( SubmitResult (Err error), Submitting input ) ->
            ( Filling input (Just <| errorToString error), Cmd.none )

        ( Edit, Submitted input ) ->
            ( Filling input Nothing, Cmd.none )

        _ ->
            ( model, Cmd.none )


encodeForServer : { input : Input, encrypted : EncryptedString } -> Value
encodeForServer arg =
    [ ( "encrypted", (\(EncryptedString es) -> Json.Encode.string es) arg.encrypted )
    , ( "captcha", Json.Encode.string arg.input.captcha )
    ]
        |> Json.Encode.object


errorToString : Http.Error -> Error
errorToString error =
    case error of
        Http.BadBody _ ->
            "Unexpected answer from the server"

        Http.BadUrl _ ->
            "Unexpected connection error"

        Http.Timeout ->
            "Timeout"

        Http.NetworkError ->
            "Connection issue"

        Http.BadStatus _ ->
            "Unexpected answer from the server"


encodeInput : Input -> Value
encodeInput input =
    [ ( "name", Json.Encode.string input.name )
    , ( "country", Json.Encode.string input.country )
    , ( "location", Json.Encode.string input.location )
    , ( "name_on_map", Json.Encode.bool input.nameOnMap )
    , ( "originally_from", Json.Encode.string input.originallyFrom )
    , ( "id", Json.Encode.string input.id )
    , ( "captcha", Json.Encode.string input.captcha )
    ]
        |> Json.Encode.object


subscriptions : Model -> Sub Msg
subscriptions _ =
    encrypted (\result -> Encrypted <| EncryptedString result)
