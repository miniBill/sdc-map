module RemoteData exposing (RemoteData(..), WebData, fromResult, toMaybe)

import Http


type RemoteData e a
    = Loading
    | Success a
    | Failure e


type alias WebData a =
    RemoteData Http.Error a


toMaybe : RemoteData e a -> Maybe a
toMaybe value =
    case value of
        Success s ->
            Just s

        Loading ->
            Nothing

        Failure _ ->
            Nothing


fromResult : Result e a -> RemoteData e a
fromResult value =
    case value of
        Ok o ->
            Success o

        Err e ->
            Failure e
