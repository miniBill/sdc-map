module Types exposing (BackendModel, BackendMsg(..), EncryptedString(..), Error, FrontendModel(..), FrontendMsg(..), Input, ToBackend(..), ToFrontend(..))

import Dict exposing (Dict)
import Random


type alias BackendModel =
    { seed : Random.Seed
    , submissions : Dict String EncryptedString
    }


type ToBackend
    = TBSubmit EncryptedString


type ToFrontend
    = TFSubmitted { id : String }


type FrontendModel
    = Filling Input (Maybe Error)
    | Encrypting Input
    | Submitting Input
    | Submitted { id : String, input : Input }


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


type FrontendMsg
    = Name String
    | Country String
    | Location String
    | NameOnMap Bool
    | Id String
    | Captcha String
    | Submit
    | Encrypted EncryptedString
    | Nop


type EncryptedString
    = EncryptedString String


type BackendMsg
    = BackendNop
