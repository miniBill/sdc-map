module Evergreen.V1.Types exposing (..)

import Dict
import Http
import Random


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
    | Dumped (Result Http.Error (Dict.Dict String EncryptedString))
    | AdminSecretKey String
    | Decrypt
    | Decrypted (List String)
    | Nop


type ToBackend
    = TBSubmit EncryptedString


type alias BackendMsg =
    {}


type ToFrontend
    = TFSubmitted
        { id : String
        }
