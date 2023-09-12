module Types exposing (Input, inputCodec)

import Serialize exposing (Codec)


type alias Input =
    { name : String
    , country : String
    , location : String
    , nameOnMap : Maybe Bool
    , id : String
    , captcha : String
    }


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
