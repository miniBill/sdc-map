module Documents exposing (LegalDocument, Paragraph, Section, cookiesDocument, privacyDocument, viewDocument)

import Date exposing (Date)
import Element exposing (Element, el, link, paddingEach, paragraph, rgb, text, textColumn)
import Element.Font as Font
import Theme
import Time exposing (Month(..))



-- Types


type alias LegalDocument =
    { title : String
    , lastUpdated : Date
    , intro : List Paragraph
    , sections : List Section
    }


type alias Section =
    { title : String
    , paragraphs : List Paragraph
    }


type alias Paragraph =
    List (Element Never)


viewParagraph : Paragraph -> Element msg
viewParagraph content =
    Element.map never <| paragraph [] content


link_ : String -> String -> Element msg
link_ label url =
    link [ Font.color <| rgb 0.2 0.2 0.8 ]
        { url = url
        , label = text label
        }


viewSection : Section -> List (Element msg)
viewSection { title, paragraphs } =
    el
        [ Font.bold
        , Font.size 28
        , paddingEach { top = 3 * Theme.rythm, left = 0, right = 0, bottom = 0 }
        ]
        (text title)
        :: List.map viewParagraph paragraphs


viewDocument : LegalDocument -> Element msg
viewDocument doc =
    textColumn [ Theme.spacing ] <|
        el
            [ Font.center
            , Font.bold
            , Font.size 36
            ]
            (text doc.title)
            :: el [ Font.semiBold ]
                (text <|
                    "Last updated: "
                        ++ Date.toIsoString doc.lastUpdated
                )
            :: List.map viewParagraph doc.intro
            ++ List.concatMap viewSection doc.sections


cookiesDocument : LegalDocument
cookiesDocument =
    { title = "Cookie Policy"
    , lastUpdated = Date.fromCalendarDate 2023 Sep 8
    , intro = [ [ text "This Cookie Policy explains: what cookies are, why this website uses cookies, and what you can do about it." ] ]
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


point : String -> List (Element msg) -> List (Element msg)
point label content =
    el [ Font.bold ] (text label) :: text " " :: content


point_ : String -> String -> List (Element msg)
point_ label content =
    point label [ text content ]


privacyDocument : LegalDocument
privacyDocument =
    let
        emailLink : Element msg
        emailLink =
            link_ "leonardo@taglialegne.it" "mailto:leonardo@taglialegne.it?subject=SDC%20map%20project"

        term : String -> Element msg
        term content =
            el [ Theme.style "font-variant" "small-caps" ] (text content)

        visibility : String -> Element msg
        visibility content =
            el [ Theme.style "font-variant" "small-caps", Font.semiBold ] (text content)
    in
    { title = "Privacy Policy"
    , lastUpdated = Date.fromCalendarDate 2023 Sep 8
    , intro =
        [ [ text "This Privacy Policy describes how and why the SDC map project collects, stores, uses and shares your information." ]
        , [ text "For any questions or concerns, contact me at ", emailLink, text "." ]
        ]
    , sections =
        [ { title = "Summary"
          , paragraphs =
                [ [ text "This is a summary, all the points are expanded on below" ]
                , point_ "What information is handled?" "The only information handled are the replies given to the survey."
                , point_ "Do we handle sensitive personal information?" "No."
                , point_ "Do we send or receive information from/to third parties?" "We only share publicly what you explicitly chose to share."
                , point_ "How do we process your information?" "The information is stored in encrypted form and only used according to your explicit choice."
                , point_ "What are your rights?" "You can ask for your information to be updated or deleted."
                , point "How do you excercise your rights?"
                    [ text "Contact me at "
                    , emailLink
                    , text " or on "
                    , link_ "Discord (miniBill)" "https://discordapp.com/users/397675122267521034"
                    , text ", "
                    , link_ "Xwitter (miniBill)" "https://twitter.com/miniBill"
                    , text ", "
                    , link_ "Telegram (miniBill)" "https://t.me/miniBill"
                    , text ", Signal, or even regular mail if you already know my address. Please refrain from using Pidgeon Post."
                    ]
                ]
          }
        , { title = "What information is handled"
          , paragraphs =
                [ [ text "In short: your replies to the survey. In more details this is how each field is handled:" ]
                , point "Name:"
                    [ text "If you select to "
                    , term "show your name on the map"
                    , text " this is the name that is used, and is thus "
                    , visibility "public"
                    , text ". If you select to "
                    , term "only use your data for statistics"
                    , text " then this is only used for data deletion and update request and is thus "
                    , visibility "private"
                    , text "."
                    ]
                , point "Country:"
                    [ text "If you select to "
                    , term "show your name on the map"
                    , text " this is used to place your marker on the map, and is thus "
                    , visibility "public"
                    , text ". If you select to "
                    , term "only use your data for statistics"
                    , text " then this is used for showing how many people come from each country and is thus "
                    , visibility "public - in aggregate form only"
                    , text "."
                    ]
                , point "Location:"
                    [ text "If you select to "
                    , term "show your name on the map"
                    , text " this is used to place your marker on the map, and is thus "
                    , visibility "public"
                    , text ". If you select to "
                    , term "only use your data for statistics"
                    , text " then this is not used and is thus "
                    , visibility "private"
                    , text ". In fact, you should avoid filling it in if you select that option."
                    ]
                , point_ "Show name on map:" "This is what determines how your data is used."
                , point "Contact"
                    [ text "This is only used for data deletion and update request and is thus "
                    , visibility "private"
                    , text "."
                    ]
                , point "Anti-bot"
                    [ text "This is only used for spam prevention and is thus "
                    , visibility "private"
                    , text "."
                    ]
                , [ text "In addition, your data is associated with a random ID that you get told after filling in the data. That ID is only used for data deletion and update request and is thus "
                  , visibility "private"
                  , text "."
                  ]
                ]
          }
        ]
    }
