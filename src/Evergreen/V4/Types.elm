module Evergreen.V4.Types exposing (..)

import Dict
import Random
import Set


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
    | AdminDecrypted (Set.Set String) (List Input)


type alias BackendModel =
    { seed : Random.Seed
    , submissions : Dict.Dict String EncryptedString
    }


type FrontendMsg
    = Name String
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
    | CaptchaIsValid String Bool
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
