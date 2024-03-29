module Types exposing (BackendModel, BackendMsg, EncryptedString(..), Error, FrontendModel(..), FrontendMsg(..), Input, ToBackend(..), ToFrontend(..), inputCodec, inputOldCodec)

import Browser exposing (UrlRequest)
import Codec
import Dict exposing (Dict)
import Random
import Serialize exposing (Codec)
import Url exposing (Url)


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
    | Privacy
    | Cookies


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
    = -- Navigation
      UrlChanged Url
    | UrlRequested UrlRequest
      -- User input
    | Name String
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


inputOldCodec : Codec.Codec Input
inputOldCodec =
    Codec.object
        (\name country location nameOnMap id captcha ->
            { name = name
            , country =
                country
                    |> String.replace "United Kingdom of Great Britain and Northern Ireland" "UK"
                    |> String.replace "United States of America" "USA"
            , location = location
            , nameOnMap = nameOnMap
            , id = id
            , captcha = captcha
            }
        )
        |> Codec.field "name" .name Codec.string
        |> Codec.field "country" .country Codec.string
        |> Codec.field "location" .location Codec.string
        |> Codec.maybeField "nameOnMap" .nameOnMap Codec.bool
        |> Codec.field "id" .id Codec.string
        |> Codec.field "captcha" .captcha Codec.string
        |> Codec.buildObject
