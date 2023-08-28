module Frontend exposing (app)

import AppUrl exposing (AppUrl)
import Browser exposing (Document, UrlRequest)
import Browser.Navigation
import Codec exposing (Codec)
import Dashboard
import Dict
import Element exposing (Element, centerX, el, fill, height, paddingEach, paragraph, shrink, text, width)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Env
import Html
import Html.Attributes
import Lamdera exposing (Url)
import PkgPorts
import Set
import Subdivisions
import Theme
import Types exposing (EncryptedString(..), FrontendModel(..), FrontendMsg(..), Input, ToBackend(..), ToFrontend(..))


app :
    { init : Lamdera.Url -> Browser.Navigation.Key -> ( FrontendModel, Cmd FrontendMsg )
    , view : FrontendModel -> Document FrontendMsg
    , update : FrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
    , updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
    , subscriptions : FrontendModel -> Sub FrontendMsg
    , onUrlRequest : UrlRequest -> FrontendMsg
    , onUrlChange : Url -> FrontendMsg
    }
app =
    Lamdera.frontend
        { init = init
        , view =
            \flagsModel ->
                { title = "SDC map"
                , body =
                    [ Element.layout
                        [ width fill
                        , height fill
                        , Theme.padding
                        ]
                        (view flagsModel)
                    ]
                }
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = \_ -> Nop
        , onUrlRequest = \_ -> Nop
        , updateFromBackend = updateFromBackend
        }


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        TFSubmitted { id } ->
            case model of
                Submitting input ->
                    ( Submitted { id = id, input = input }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        TFAdmin dict ->
            ( AdminDecrypting "" dict, Cmd.none )


init : Lamdera.Url -> Browser.Navigation.Key -> ( FrontendModel, Cmd FrontendMsg )
init url navKey =
    let
        appUrl : AppUrl
        appUrl =
            AppUrl.fromUrl url

        defaultModel : FrontendModel
        defaultModel =
            Filling
                { name = ""
                , country = ""
                , location = ""
                , nameOnMap = Nothing
                , captcha = ""
                , id = ""
                }
                Nothing
    in
    case Dict.get "key" appUrl.queryParameters of
        Just [ key ] ->
            ( defaultModel
            , Cmd.batch
                [ Lamdera.sendToBackend <| TBAdmin key
                , case Env.mode of
                    Env.Production ->
                        Browser.Navigation.replaceUrl navKey "/admin"

                    Env.Development ->
                        Cmd.none
                ]
            )

        _ ->
            ( defaultModel
            , Cmd.none
            )


view : FrontendModel -> Element FrontendMsg
view model =
    case model of
        AdminDecrypting key _ ->
            Theme.column []
                [ Input.currentPassword []
                    { onChange = AdminSecretKey
                    , placeholder = Nothing
                    , label = Input.labelAbove [] <| text "Secret key"
                    , show = False
                    , text = key
                    }
                , Theme.button []
                    { onPress = Just Decrypt
                    , label = text "Decrypt"
                    }
                ]

        AdminDecrypted captchas inputs ->
            Dashboard.view captchas inputs

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
                    Theme.button []
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

        Submitted { input, id } ->
            Theme.column
                [ Theme.padding
                , centerX
                ]
                [ viewInputReadonly input
                , paragraph []
                    [ text "Thank you for your submission 😊" ]
                , paragraph []
                    [ text <|
                        "You can ask for your data to be removed using your contact info, or with this id: "
                            ++ id
                    ]
                ]


isValid : Input -> Bool
isValid { name, country, captcha, nameOnMap } =
    (String.isEmpty name
        || String.isEmpty country
        || String.isEmpty captcha
        || (nameOnMap == Nothing)
    )
        |> not


viewInputReadonly : Input -> Element FrontendMsg
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
                (case input.nameOnMap of
                    Just True ->
                        "Show on map"

                    Just False ->
                        "Only statistics"

                    Nothing ->
                        ""
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


viewError : String -> Element FrontendMsg
viewError message =
    ("Error: " ++ message)
        |> text
        |> el [ Background.color <| Element.rgb 1 0.8 0.8 ]


viewInput : Input -> Element FrontendMsg
viewInput input =
    let
        inputRow : String -> String -> String -> Bool -> (String -> FrontendMsg) -> String -> List String -> List (Element FrontendMsg)
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

        yesNoRow : String -> String -> Bool -> (Bool -> FrontendMsg) -> Maybe Bool -> Element FrontendMsg
        yesNoRow label description mandatory toMsg value =
            Input.radioRow [ Theme.spacing ]
                { label = toLabel label description mandatory
                , selected = value
                , onChange = toMsg
                , options =
                    [ Input.option True <| text "Show on map"
                    , Input.option False <| text "Only statistics"
                    ]
                }

        toLabel : String -> String -> Bool -> Input.Label FrontendMsg
        toLabel label description mandatory =
            Input.labelAbove
                [ paddingEach
                    { top = 0
                    , left = 0
                    , right = 0
                    , bottom = Theme.rythm // 2
                    }
                ]
            <|
                paragraph []
                    [ text label
                    , if mandatory then
                        el [ Font.color <| Element.rgb 1 0 0 ] <| text "*"

                      else
                        Element.none
                    , text " "
                    , el [ Font.size 14, Font.color <| Element.rgb 0.4 0.4 0.4 ] <| text description
                    ]

        countries : List String
        countries =
            Dict.keys Subdivisions.subdivisions

        locations : List String
        locations =
            Dict.get input.country Subdivisions.subdivisions
                |> Maybe.withDefault []
                |> List.sort
    in
    [ inputRow "name" "Name" "" True Name input.name []
    , inputRow "country" "Country" "" True Country input.country countries
    , inputRow "location" "Location" "Where are you from? Pick the name of the region, county, or a big city near you. DON'T provide your address." False Location input.location locations
    , [ yesNoRow "Show name on map" "Should your name be shown on the map, or just used for statistics?" True NameOnMap input.nameOnMap ]
    , inputRow "contact" "Contact" "Some contact (email/twitter/discord/...) you can use to prove it's you if you want to remove your information. Will not be published in any form." False Id input.id []
    , inputRow "captcha" "Anti-bot" "What is the title of her first album?" True Captcha input.captcha []
    ]
        |> List.intersperse [ text " " ]
        |> List.concat
        |> Theme.column []


update : FrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
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
            ( Filling { input | nameOnMap = Just nameOnMap } maybeError, Cmd.none )

        ( Captcha captcha, Filling input maybeError ) ->
            ( Filling { input | captcha = captcha } maybeError, Cmd.none )

        ( Submit, Filling input _ ) ->
            ( Encrypting input
            , PkgPorts.encrypt
                { input = encodeInput input
                , serverPublic = Env.serverPublicKey
                }
            )

        ( Encrypted encryptedString, Encrypting input ) ->
            ( Submitting input
            , Lamdera.sendToBackend <| TBSubmit encryptedString
            )

        ( AdminSecretKey key, AdminDecrypting _ dict ) ->
            ( AdminDecrypting key dict, Cmd.none )

        ( Decrypt, AdminDecrypting key dict ) ->
            ( model
            , PkgPorts.decrypt
                { inputs = List.map (\(EncryptedString e) -> e) (Dict.values dict)
                , serverSecret = key
                }
            )

        ( Decrypted inputs, _ ) ->
            ( inputs
                |> List.filterMap decodeInput
                |> AdminDecrypted Set.empty
            , Cmd.none
            )

        ( CaptchaIsValid captcha valid, AdminDecrypted invalidCaptchas inputs ) ->
            ( if valid then
                AdminDecrypted (Set.remove captcha invalidCaptchas) inputs

              else
                AdminDecrypted (Set.insert captcha invalidCaptchas) inputs
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


encodeInput : Input -> String
encodeInput input =
    Codec.encodeToString 0 inputCodec input


decodeInput : String -> Maybe Input
decodeInput string =
    Codec.decodeString inputCodec string
        |> Result.toMaybe
        |> Maybe.map
            (\input ->
                { input
                    | country =
                        input.country
                            |> String.replace "United Kingdom of Great Britain and Northern Ireland" "UK"
                            |> String.replace "United States of America" "USA"
                }
            )


inputCodec : Codec Input
inputCodec =
    Codec.object
        (\name country location nameOnMap id captcha ->
            { name = name
            , country = country
            , location = location
            , nameOnMap = nameOnMap
            , id = id
            , captcha = captcha
            }
        )
        |> Codec.field "name" .name Codec.string
        |> Codec.field "country" .country Codec.string
        |> Codec.field "location" .location Codec.string
        |> Codec.maybeField "name_on_map" .nameOnMap Codec.bool
        |> Codec.field "id" .id Codec.string
        |> Codec.field "captcha" .captcha Codec.string
        |> Codec.buildObject


subscriptions : FrontendModel -> Sub FrontendMsg
subscriptions _ =
    Sub.batch
        [ PkgPorts.encrypted (\result -> Encrypted <| EncryptedString result)
        , PkgPorts.decrypted Decrypted
        ]
