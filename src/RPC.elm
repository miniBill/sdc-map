module RPC exposing (lamdera_handleEndpoints, requestDump)

import Dict exposing (Dict)
import Env
import Http
import Json.Decode as JD
import Json.Encode as JE exposing (Value)
import Lamdera exposing (SessionId)
import Lamdera.Wire3 as Wire3
import LamderaRPC
import Task exposing (Task)
import Types exposing (BackendModel, EncryptedString(..))


dump : SessionId -> BackendModel -> String -> ( Result Http.Error (Dict String EncryptedString), BackendModel, Cmd msg )
dump _ model key =
    ( if key == Env.key then
        Ok model.submissions

      else
        Err <| Http.BadStatus 403
    , model
    , Cmd.none
    )


dumpJson : SessionId -> BackendModel -> Value -> ( Result Http.Error Value, BackendModel, Cmd msg )
dumpJson _ model body =
    case JD.decodeValue JD.string body of
        Ok key ->
            ( if key == Env.key then
                let
                    encoder : Dict String EncryptedString -> Value
                    encoder =
                        JE.dict identity (\(EncryptedString e) -> JE.string e)
                in
                Ok <| encoder model.submissions

              else
                Err <| Http.BadStatus 403
            , model
            , Cmd.none
            )

        Err err ->
            ( Err <|
                Http.BadBody <|
                    "Failed to decode arg for [json] "
                        ++ "exampleJson "
                        ++ JD.errorToString err
            , model
            , Cmd.none
            )



-- Things that should be auto-generated in future


requestDump : String -> Task Http.Error (Dict String EncryptedString)
requestDump value =
    LamderaRPC.asTask Wire3.encodeString
        (Wire3.decodeDict Wire3.decodeString Types.w3_decode_EncryptedString)
        value
        "dump"


lamdera_handleEndpoints : LamderaRPC.RPCArgs -> BackendModel -> ( LamderaRPC.RPCResult, BackendModel, Cmd msg )
lamdera_handleEndpoints args model =
    case args.endpoint of
        "dump" ->
            LamderaRPC.handleEndpoint
                dump
                Wire3.decodeString
                (Wire3.encodeDict Wire3.encodeString Types.w3_encode_EncryptedString)
                args
                model

        "dumpJson" ->
            LamderaRPC.handleEndpointJson dumpJson args model

        _ ->
            ( LamderaRPC.ResultFailure <| Http.BadBody <| "Unknown endpoint " ++ args.endpoint, model, Cmd.none )
