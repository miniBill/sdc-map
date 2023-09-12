module Types exposing (Country, Input, Location, Model, Msg(..), findPosition, inputCodec, normalizeCountry)

import Dict exposing (Dict)
import GeoJson exposing (Geometry, Position)
import Http
import List.Extra
import RemoteData exposing (RemoteData(..), WebData)
import Serialize exposing (Codec)
import Set exposing (Set)


type alias Model =
    { invalidCaptchas : Set String
    , inputs : List Input
    , indexData : WebData (Dict Country { threeLetterCode : String, level : Int })
    , geoJsonData : Dict Country (RemoteData (Maybe Http.Error) (List Location))
    , capitals : WebData (Dict Country Position)
    }


type alias Country =
    String


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


type alias Location =
    { name : String
    , alternativeNames : List String
    , geometry : Geometry
    , center : Position
    }


type Msg
    = InvalidCaptchas (Set String)
    | GotIndexData (Result Http.Error (Dict Country { threeLetterCode : String, level : Int }))
    | GotCapitalsData (Result Http.Error (Dict Country Position))
    | GotGeoJson Country (Result (Maybe Http.Error) (List Location))
    | ReloadCountry Country
    | Download


findPosition : Model -> { a | country : String, location : String } -> Result String Position
findPosition model { country, location } =
    if String.isEmpty location then
        case model.capitals of
            Loading ->
                Err "Loading"

            Failure _ ->
                Err "Failure"

            Success capitals ->
                case Dict.get country capitals of
                    Nothing ->
                        Err "Missing (capitals)"

                    Just capital ->
                        Ok capital

    else
        case Dict.get (normalizeCountry country) model.geoJsonData of
            Nothing ->
                Err "Missing (data)"

            Just Loading ->
                Err "Loading"

            Just (Failure Nothing) ->
                Err "Missing (index)"

            Just (Failure _) ->
                Err "Failure"

            Just (Success locations) ->
                case locations of
                    [] ->
                        Err "No jsons loaded"

                    _ ->
                        let
                            normalized : String
                            normalized =
                                normalize location

                            normalize : String -> String
                            normalize s =
                                String.toLower s |> String.replace " " ""
                        in
                        locations
                            |> List.Extra.findMap
                                (\loc ->
                                    if
                                        (normalize loc.name == normalized)
                                            || List.member
                                                normalized
                                                (List.map normalize loc.alternativeNames)
                                    then
                                        Just (Ok loc.center)

                                    else
                                        Nothing
                                )
                            |> Maybe.withDefault (Err "Not found")


normalizeCountry : String -> String
normalizeCountry =
    String.replace "UK" "United Kingdom"
