module Documents exposing (Inline, LegalDocument, Paragraph, Section, cookiesDocument, privacyDocument, view)

import Date exposing (Date)
import Element exposing (Element, centerX, el, fill, link, paddingEach, paragraph, rgb, text, textColumn, width)
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
    List Inline


type Inline
    = Text String
    | Term String
    | Visibility String
    | Link String String
    | Bold String



-- View


view : LegalDocument -> Element msg
view doc =
    let
        title : Element msg
        title =
            el
                [ Font.center
                , Font.bold
                , Font.size 36
                ]
                (text doc.title)

        lastUpdated : List Inline
        lastUpdated =
            [ Bold "Last updated: ", Text <| Date.toIsoString doc.lastUpdated ++ "." ]
    in
    textColumn
        [ Theme.spacing
        , width <| Element.maximum 700 fill
        , centerX
        ]
        (title
            :: List.map viewParagraph (lastUpdated :: doc.intro)
            ++ List.concatMap viewSection doc.sections
        )


viewSection : Section -> List (Element msg)
viewSection { title, paragraphs } =
    paragraph
        [ Font.bold
        , Font.size 28
        , paddingEach { top = 3 * Theme.rythm, left = 0, right = 0, bottom = 0 }
        ]
        [ text title ]
        :: List.map viewParagraph paragraphs


viewParagraph : Paragraph -> Element msg
viewParagraph content =
    paragraph [] (List.map viewInline content)


viewInline : Inline -> Element msg
viewInline inline =
    case inline of
        Text content ->
            text content

        Term content ->
            el [ Theme.style "font-variant" "small-caps" ] (text content)

        Visibility content ->
            el [ Theme.style "font-variant" "small-caps", Font.bold ] (text content)

        Link label url ->
            link [ Font.color <| rgb 0.2 0.2 0.8 ]
                { url = url
                , label = text label
                }

        Bold content ->
            el [ Font.bold ] (text content)



-- Shortcuts


emailLink : Inline
emailLink =
    Link "leonardo@taglialegne.it" "mailto:leonardo@taglialegne.it?subject=SDC%20map%20project"


point : String -> List Inline -> List Inline
point label content =
    Bold label :: Text " " :: content


point_ : String -> String -> List Inline
point_ label content =
    point label [ Text content ]



-- Documents


cookiesDocument : LegalDocument
cookiesDocument =
    { title = "🍪 Cookie Policy"
    , lastUpdated = Date.fromCalendarDate 2023 Sep 8
    , intro =
        [ [ Text "This Cookie Policy explains: what cookies are, why this website uses cookies, and what you can do about it." ]
        , [ Text "For any questions or concerns, contact us at ", emailLink, Text "." ]
        ]
    , sections =
        [ { title = "🗡️ Oh Cookie, Cookie, wherefore art thou a Cookie?"
          , paragraphs =
                [ [ Text "A cookie is a small piece of data that is stored on your device when you visit a website. They are used to identify your machine. They can also potentially be used to track you across different websites." ]
                ]
          }
        , { title = "😕 Y tho"
          , paragraphs =
                [ [ Text "The SDC map project uses Lamdera as a hosting platform. Lamdera uses a session cookie to recognize different tabs from the same browser, and to recognize the same browser connecting again in the future. The SDC map project does not use this information at all." ]
                ]
          }
        , { title = "🏎️ Power is nothing without a Nintendo™ Switch™ controller"
          , paragraphs =
                [ [ Text "You cannot reject the session cookie as is technically necessary for the website to function. Feel free to delete it after you've used the website. By default it will expire automatically in 30 days." ]
                , [ Text "You can find information on how to yeet cookies in "
                  , Link "Firefox" "https://support.mozilla.org/en-US/kb/clear-cookies-and-site-data-firefox"
                  , Text ", "
                  , Link "Chrome" "https://support.google.com/chrome/answer/95647"
                  , Text ", "
                  , Link "Safari" "https://support.apple.com/en-ie/guide/safari/sfri11471/mac"
                  , Text ", "
                  , Link "Edge" "https://support.microsoft.com/en-us/microsoft-edge/delete-cookies-in-microsoft-edge-63947406-40ac-c3b8-57b9-2a946a29ae09"
                  , Text ", "
                  , Link "Opera" "https://help.opera.com/en/latest/web-preferences/"
                  , Text "."
                  ]
                ]
          }
        , { title = "☕ What else?"
          , paragraphs =
                [ [ Text "The SDC map project does not use any other tracking technology, such as Flash Cookies, local storage, web beacons or illithid larvæ." ]
                ]
          }
        ]
    }


privacyDocument : LegalDocument
privacyDocument =
    { title = "🕵️ Privacy Policy"
    , lastUpdated = Date.fromCalendarDate 2023 Sep 8
    , intro =
        [ [ Text "This Privacy Policy describes how and why the SDC map project collects, stores, uses and shares your information." ]
        , [ Text "The information will be used to create a map of the SDC and to calculate statistics on which countries are represented." ]
        , [ Text "For any questions or concerns, contact us at ", emailLink, Text "." ]
        ]
    , sections =
        [ summary
        , whatInformation
        , process
        , thirdParties
        , yourRights
        , exerciseRights
        , cccChanges
        ]
    }


summary : Section
summary =
    { title = "📝 Summary"
    , paragraphs =
        [ [ Text "This is a summary, the information is expanded in further sections." ]
        , point_ "What information is handled?" "The only information handled are the replies given to the survey."
        , point_ "Do we handle sensitive personal information?" "No."
        , point_ "How long do we keep the information?" "As long as you don't ask for its deletion."
        , point_ "How do we process your information?" "The information is stored in encrypted form and only used according to your explicit choice."
        , point_ "Do we send or receive information from/to third parties?" "We only share publicly what you explicitly chose to share."
        , point_ "What are your rights?" "You can ask for your information to be updated or deleted."
        , point_ "How do you exercise your rights?" "Contact me for any request."
        ]
    }


whatInformation : Section
whatInformation =
    { title = "👀 What information is handled"
    , paragraphs =
        [ [ Text "In short: your replies to the survey. In more details this is how each field is handled, depending on what you choose for the "
          , Term "Show name on map"
          , Text " question:"
          ]
        , point "Name:"
            [ Text "If you select to "
            , Term "show your name on the map"
            , Text " this is the name that is used, and is thus "
            , Visibility "public"
            , Text ". If you select to "
            , Term "only use your data for statistics"
            , Text " then this is only used for data deletion and update request and is thus "
            , Visibility "private"
            , Text "."
            ]
        , point "Country:"
            [ Text "If you select to "
            , Term "show your name on the map"
            , Text " this is used to place your marker on the map, and is thus "
            , Visibility "public"
            , Text ". If you select to "
            , Term "only use your data for statistics"
            , Text " then this is used for showing how many people come from each country and is thus "
            , Visibility "public - in aggregate form only"
            , Text "."
            ]
        , point "Location:"
            [ Text "If you select to "
            , Term "show your name on the map"
            , Text " this is used to place your marker on the map, and is thus "
            , Visibility "public"
            , Text ". Even if you select to "
            , Term "show your name on the map"
            , Text " you are free to leave this empty. If you select to "
            , Term "only use your data for statistics"
            , Text " then this is not used and is thus "
            , Visibility "private"
            , Text ". In fact, you should avoid filling it in if you select that option."
            ]
        , point_ "Show name on map:" "This is what determines how your data is used."
        , point "Contact"
            [ Text "This is only used for data deletion and update request and is thus "
            , Visibility "private"
            , Text "."
            ]
        , point "Anti-bot"
            [ Text "This is only used for spam prevention and is thus "
            , Visibility "private"
            , Text "."
            ]
        , [ Text "In addition, your data is associated with a random ID that you get told after filling in the data. That ID is only used for data deletion and update request and is thus "
          , Visibility "private"
          , Text "."
          ]
        ]
    }


process : Section
process =
    { title = "🖥️ How do we process your information?"
    , paragraphs =
        [ [ Text "Your answers are encrypted in your browser, before being sent to the server, and are only readable by me (Leonardo Taglialegne) - in particular, they are not readable by Cofoundry Ltd (the owner of the Lamdera platform), nor by any other party that has access to the server." ]
        , [ Text "The key needed to read the data is not stored on the server, and never communicated to third parties." ]
        , [ Text "The encrypted data is stored on a server in Germany." ]
        , [ Text "The ID is not encrypted, as this allows deleting an answer without having to decrypt it. The ID is a random string, and does not contain personal information." ]
        , [ Text "The information will be used to create a map, with a pin for each person who has chosen to "
          , Term "show their name on the map"
          , Text ". It will also be used to create a table detailing how many people selected each country out of all the answers."
          ]
        , [ Text "The information will be vetted and made uniform before publication:"
          ]
        , [ Text "Country and location names will be made uniform (examples: UK and United Kingdom; US, USA and United States of America; Netherlands and The Netherlands; ...), location data will be made less precise if needed (example: removing street addresses or borough information)."
          ]
        , [ Text "Names might also be edited if needed (examples: removing profanities, spam, html tags, ...)."
          ]
        , [ Text "The contact information is only used in case of an update or delete request without an ID." ]
        , [ Text "The ", Term "private", Text " information is never stored in unencrypted form." ]
        , [ Text "The information may be copied and stored for backup purposes, only in encrypted form." ]
        ]
    }


thirdParties : Section
thirdParties =
    { title = "☁️ Do we send or receive information from/to third parties?"
    , paragraphs =
        [ [ Text "The information is sent - in encrypted form - to the server, which is managed by Cofoundry Ltd." ]
        , [ Text "The results of the project - the map and the statistics - will be published publicly." ]
        , [ Text "We don't receive information from third parties." ]
        ]
    }


yourRights : Section
yourRights =
    { title = "⚖️ What are your rights?"
    , paragraphs =
        [ [ Text "You can ask for your information to be updated or deleted. This includes updating your choice of how your data can be used." ]
        , [ Text "If you do so, the public map and statistics will be updated, and the information will be updated or deleted from any backups we made." ]
        , [ Text "That said, once the information is public, it could be copied, stored, and disseminated by third parties, so be wary of what you share." ]
        ]
    }


exerciseRights : Section
exerciseRights =
    { title = "📬 How do you exercise your rights?"
    , paragraphs =
        [ [ Text "Contact me at "
          , emailLink
          , Text " or on "
          , Link "Discord (miniBill)" "https://discordapp.com/users/397675122267521034"
          , Text ", "
          , Link "Xwitter (miniBill)" "https://twitter.com/miniBill"
          , Text ", "
          , Link "Telegram (miniBill)" "https://t.me/miniBill"
          , Text ", Signal, or even regular mail if you already know my address. Please refrain from using Pidgeon Post."
          ]
        , [ Text "You will need to provide the random ID, or otherwise prove your identity (as an example: by contacting me from the account you specified in your contact information)." ]
        ]
    }


cccChanges : Section
cccChanges =
    { title = "♻️ Changes to this policy"
    , paragraphs =
        [ [ Text "Changes to this policy will be available at "
          , Link "this same address" "/privacy"
          , Text "."
          ]
        , [ Text "If you want to get notified in case of a change, contact me and I will notify you." ]
        ]
    }
