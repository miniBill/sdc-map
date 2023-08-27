port module Main exposing (Flags, Model, Msg, main)

import Browser
import Dict
import Element exposing (Element, centerX, el, fill, height, paragraph, shrink, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html
import Html.Attributes
import Http
import Json.Encode exposing (Value)
import Subdivisions
import Theme


port encrypt : Value -> Cmd msg


port encrypted : (String -> msg) -> Sub msg


type alias Input =
    { name : String
    , country : String
    , location : String
    , nameOnMap : Bool
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
                Element.layout
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
                        Element.none
                , if isValid input then
                    Input.button
                        [ Theme.padding
                        , Border.width 1
                        ]
                        { onPress = Just Submit
                        , label = text "Submit"
                        }

                  else
                    Element.none
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
                    , Input.button []
                        { onPress = Just Edit
                        , label =
                            el [ Font.underline ] <|
                                text "edit and submit again."
                        }
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
        inputRow : String -> String -> ( Element msg, Element a )
        inputRow label value =
            ( el [ Font.alignRight ] <| text <| label ++ ": "
            , el [ Font.bold ] <| text value
            )
    in
    Element.table [ Theme.spacing ]
        { data =
            [ inputRow "Name" input.name
            , inputRow "Country" input.country
            , inputRow "Location" input.location
            , inputRow "Name on map"
                (if input.nameOnMap then
                    "Yes"

                 else
                    "No"
                )
            , inputRow "Id" input.id
            ]
        , columns =
            [ { header = Element.none
              , view = Tuple.first
              , width = shrink
              }
            , { header = Element.none
              , view = Tuple.second
              , width = fill
              }
            ]
        }


viewError : String -> Element Msg
viewError message =
    ("Error: " ++ message)
        |> text
        |> el [ Background.color <| Element.rgb 1 0.8 0.8 ]


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
                    |> List.map Element.htmlAttribute
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
                |> Element.html
            ]

        checkboxRow : String -> String -> Bool -> (Bool -> Msg) -> Bool -> Element Msg
        checkboxRow label description mandatory toMsg value =
            Input.checkbox []
                { label = toLabel label description mandatory
                , checked = value
                , onChange = toMsg
                , icon = Input.defaultCheckbox
                }

        toLabel : String -> String -> Bool -> Input.Label Msg
        toLabel label description mandatory =
            Input.labelAbove [] <|
                paragraph []
                    [ text label
                    , if mandatory then
                        el [ Font.color <| Element.rgb 1 0 0 ] <| text "*"

                      else
                        Element.none
                    , text " "
                    , el [ Font.size 14, Font.color <| Element.rgb 0.6 0.6 0.6 ] <| text description
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
    , ( "id", Json.Encode.string input.id )
    , ( "captcha", Json.Encode.string input.captcha )
    ]
        |> Json.Encode.object


subscriptions : Model -> Sub Msg
subscriptions _ =
    encrypted (\result -> Encrypted <| EncryptedString result)
