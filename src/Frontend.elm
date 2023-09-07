module Frontend exposing (app)

import AppUrl exposing (AppUrl)
import Base64
import Browser exposing (Document, UrlRequest)
import Browser.Navigation
import Codec
import Date exposing (Date)
import Dict
import Element exposing (Element, alignBottom, centerX, column, el, fill, height, link, paddingEach, paragraph, rgb, rgb255, shrink, spacing, text, textColumn, width)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Env
import Flate
import Html
import Html.Attributes
import Lamdera exposing (Url)
import PkgPorts
import Serialize
import Subdivisions
import Theme
import Time exposing (Month(..))
import Types exposing (EncryptedString(..), FrontendModel(..), FrontendMsg(..), Input, ToBackend(..), ToFrontend(..))
import Url


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
                        , Background.color <| rgb255 0xC0 0xBD 0xB6
                        , Font.color <| rgb255 0x27 0x28 0x1A
                        ]
                        (Theme.column [ width fill, height fill ]
                            [ view flagsModel
                            , footer
                            ]
                        )
                    ]
                }
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = \_ -> Nop
        , onUrlRequest = UrlRequested
        , updateFromBackend = updateFromBackend
        }


footer : Element msg
footer =
    Theme.row [ centerX, alignBottom ]
        [ link [] { url = "/privacy", label = text "🕵️ Privacy policy" }
        , link [] { url = "/cookies", label = text "🍪 Cookie policy" }
        ]


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
    case appUrl.path of
        [ "cookies" ] ->
            ( Cookies, Cmd.none )

        [ "privacy" ] ->
            ( Privacy, Cmd.none )

        _ ->
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
        Privacy ->
            viewDocument privacyDocument

        Cookies ->
            viewDocument cookiesDocument

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

        AdminDecrypted inputs ->
            inputs
                |> Serialize.encodeToBytes (Serialize.list Types.inputCodec)
                |> Flate.deflate
                |> Base64.fromBytes
                |> Maybe.withDefault "<failed>"
                |> text
                |> List.singleton
                |> paragraph []

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


type alias LegalDocument =
    { title : String
    , lastUpdated : Date
    , intro : Paragraph
    , sections : List Section
    }


type alias Section =
    { title : String
    , paragraphs : List Paragraph
    }


type alias Paragraph =
    List (Element Never)


viewDocument : LegalDocument -> Element msg
viewDocument doc =
    column [ centerX, spacing 30 ]
        ([ textColumn [ width fill, Theme.spacing ]
            [ el
                [ Font.center
                , Font.bold
                , Font.size 30
                ]
                (text doc.title)
            , el [ Font.semiBold ] <|
                text <|
                    "Last updated: "
                        ++ Date.toIsoString doc.lastUpdated
            ]
         , textColumn [] [ viewParagraph doc.intro ]
         ]
            ++ List.map viewSection doc.sections
        )


viewSection : Section -> Element msg
viewSection { title, paragraphs } =
    textColumn [ Theme.spacing ] <|
        el
            [ Font.bold
            , Font.size 24
            ]
            (text title)
            :: List.map viewParagraph paragraphs


viewParagraph : Paragraph -> Element msg
viewParagraph content =
    Element.map never <| paragraph [] content


link_ : String -> String -> Element msg
link_ label url =
    link [ Font.color <| rgb 0 0 1 ]
        { url = url
        , label = text label
        }


cookiesDocument : LegalDocument
cookiesDocument =
    { title = "Cookie Policy"
    , lastUpdated = Date.fromCalendarDate 2023 Sep 7
    , intro = [ text "This Cookie Policy explains: what cookies are, why this website uses cookies, and what you can do about it." ]
    , sections =
        [ { title = "Oh Cookie, Cookie, wherefore art thou a Cookie?"
          , paragraphs =
                [ [ text "A cookie is a small piece of data that is stored on your device when you visit a website. They are used to identify your machine. They can also potentially be used to track you across different websites." ]
                ]
          }
        , { title = "Y tho"
          , paragraphs =
                [ [ text "The SDC map project uses Lamdera as a hosting platform. Lamdera uses a session cookie to recognize different tabs from the same browser, and to recognize the same browser connecting again in the future. The SDC map project does not use this information at all." ]
                ]
          }
        , { title = "Power is nothing without a Nintendo™ Switch™ controller"
          , paragraphs =
                [ [ text "You cannot reject the session cookie as is technically necessary for the website to function. Feel free to delete it after you've used the website. By default it will expire automatically in 30 days." ]
                , [ text "You can find information on how to yeet cookies in "
                  , link_ "Firefox" "https://support.mozilla.org/en-US/kb/clear-cookies-and-site-data-firefox"
                  , text ", "
                  , link_ "Chrome" "https://support.google.com/chrome/answer/95647"
                  , text ", "
                  , link_ "Safari" "https://support.apple.com/en-ie/guide/safari/sfri11471/mac"
                  , text ", "
                  , link_ "Edge" "https://support.microsoft.com/en-us/microsoft-edge/delete-cookies-in-microsoft-edge-63947406-40ac-c3b8-57b9-2a946a29ae09"
                  , text ", "
                  , link_ "Opera" "https://help.opera.com/en/latest/web-preferences/"
                  , text "."
                  ]
                ]
          }
        , { title = "What else?", paragraphs = [ [ text "The SDC map project does not use any other tracking technology, such as Flash Cookies, local storage, web beacons or illithid larvæ." ] ] }
        ]
    }


privacyDocument : LegalDocument
privacyDocument =
    { title = "Privacy Policy"
    , lastUpdated = Date.fromCalendarDate 2023 Sep 7
    , intro = []
    , sections = []
    }


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
                (([ Html.Attributes.autocomplete True
                  , Html.Attributes.name autocomplete
                  , Html.Attributes.id autocomplete
                  , if List.isEmpty autocompleteList then
                        Html.Attributes.classList []

                    else
                        Html.Attributes.list (autocomplete ++ "-list")
                  ]
                    |> List.map Element.htmlAttribute
                 )
                    ++ [ Background.color <| rgb255 0xE7 0xE8 0xED ]
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
                    , bottom = Theme.rythm
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
        ( UrlRequested (Browser.Internal url), _ ) ->
            ( model
            , Browser.Navigation.load (Url.toString url)
            )

        ( UrlRequested (Browser.External url), _ ) ->
            ( model
            , Browser.Navigation.load url
            )

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
                |> AdminDecrypted
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


encodeInput : Input -> String
encodeInput input =
    Serialize.encodeToString Types.inputCodec input


decodeInput : String -> Maybe Input
decodeInput string =
    case Serialize.decodeFromString Types.inputCodec string of
        Err _ ->
            case Codec.decodeString Types.inputOldCodec string of
                Ok input ->
                    Just input

                Err _ ->
                    Nothing

        Ok input ->
            Just input


subscriptions : FrontendModel -> Sub FrontendMsg
subscriptions _ =
    Sub.batch
        [ PkgPorts.encrypted (\result -> Encrypted <| EncryptedString result)
        , PkgPorts.decrypted Decrypted
        ]
