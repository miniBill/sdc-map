module Types exposing (BackendModel, BackendMsg, EncryptedString(..), Error, FrontendModel(..), FrontendMsg(..), Input, ToBackend(..), ToFrontend(..), inputCodec)

import Dict exposing (Dict)
import Random
import Serialize exposing (Codec)


type alias BackendModel =
    { seed : Random.Seed
    , submissions : Dict String EncryptedString
    }


type ToBackend
    = TBSubmit EncryptedString
    | TBAdmin String


type ToFrontend
    = TFSubmitted { id : String }
    | TFAdmin (Dict String EncryptedString)


type FrontendModel
    = Filling Input (Maybe Error)
    | Encrypting Input
    | Submitting Input
    | Submitted { id : String, input : Input }
    | AdminDecrypting String (Dict String EncryptedString)
    | AdminDecrypted (List Input)


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
      -- Admin page
    | AdminSecretKey String
    | Decrypt
    | Decrypted (List String)
      -- Nop
    | Nop


type EncryptedString
    = EncryptedString String


type alias BackendMsg =
    {}


inputCodec : Codec e Input
inputCodec =
    Serialize.record
        (\name country location nameOnMap id captcha ->
            { name = name
            , country = country
            , location = location
            , nameOnMap = nameOnMap
            , id = id
            , captcha = captcha
            }
        )
        |> Serialize.field .name Serialize.string
        |> Serialize.field .country Serialize.string
        |> Serialize.field .location Serialize.string
        |> Serialize.field .nameOnMap (Serialize.maybe Serialize.bool)
        |> Serialize.field .id Serialize.string
        |> Serialize.field .captcha Serialize.string
        |> Serialize.finishRecord
