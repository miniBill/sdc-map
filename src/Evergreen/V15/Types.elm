module Evergreen.V15.Types exposing (..)

import Browser
import Dict
import Random
import Url


type alias Input =
    { name : String
    , country : String
    , location : String
    , nameOnMap : Maybe Bool
    , id : String
    , captcha : String
    }


type alias Error =
    String


type EncryptedString
    = EncryptedString String


type FrontendModel
    = Filling Input (Maybe Error)
    | Encrypting Input
    | Submitting Input
    | Submitted
        { id : String
        , input : Input
        }
    | AdminDecrypting String (Dict.Dict String EncryptedString)
    | AdminDecrypted (List Input)
    | Privacy
    | Cookies


type alias BackendModel =
    { seed : Random.Seed
    , submissions : Dict.Dict String EncryptedString
    }


type FrontendMsg
    = UrlChanged Url.Url
    | UrlRequested Browser.UrlRequest
    | Name String
    | Country String
    | Location String
    | NameOnMap Bool
    | Id String
    | Captcha String
    | Submit
    | Encrypted EncryptedString
    | AdminSecretKey String
    | Decrypt
    | Decrypted (List String)
    | Nop


type ToBackend
    = TBSubmit EncryptedString
    | TBAdmin String


type alias BackendMsg =
    {}


type ToFrontend
    = TFSubmitted
        { id : String
        }
    | TFAdmin (Dict.Dict String EncryptedString)
