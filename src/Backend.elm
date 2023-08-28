module Backend exposing (app)

import Dict
import Env
import Lamdera
import Random
import Types exposing (BackendModel, BackendMsg, ToBackend(..), ToFrontend(..))


app :
    { init : ( BackendModel, Cmd BackendMsg )
    , update : BackendMsg -> BackendModel -> ( BackendModel, Cmd BackendMsg )
    , updateFromFrontend : Lamdera.SessionId -> Lamdera.ClientId -> ToBackend -> BackendModel -> ( BackendModel, Cmd BackendMsg )
    , subscriptions : BackendModel -> Sub BackendMsg
    }
app =
    Lamdera.backend
        { init = init
        , subscriptions = subscriptions
        , update = update
        , updateFromFrontend = updateFromFrontend
        }


init : ( BackendModel, Cmd BackendMsg )
init =
    ( { seed = Random.initialSeed Env.seed
      , submissions = Dict.empty
      }
    , Cmd.none
    )


subscriptions : BackendModel -> Sub BackendMsg
subscriptions _ =
    Sub.none


update : BackendMsg -> BackendModel -> ( BackendModel, Cmd BackendMsg )
update _ model =
    ( model, Cmd.none )


updateFromFrontend : Lamdera.SessionId -> Lamdera.ClientId -> ToBackend -> BackendModel -> ( BackendModel, Cmd BackendMsg )
updateFromFrontend _ clientId msg model =
    case msg of
        TBSubmit encrypted ->
            let
                ( id, newSeed ) =
                    Random.step idGenerator model.seed
            in
            ( { model
                | seed = newSeed
                , submissions = Dict.insert id encrypted model.submissions
              }
            , Lamdera.sendToFrontend clientId <| TFSubmitted { id = id }
            )

        TBAdmin key ->
            if key == Env.key then
                ( model, Lamdera.sendToFrontend clientId <| TFAdmin model.submissions )

            else
                ( model, Cmd.none )


idGenerator : Random.Generator String
idGenerator =
    Random.map4
        (\arg1 arg2 arg3 arg4 ->
            [ arg1, arg2, arg3, arg4 ]
                |> List.map String.fromInt
                |> String.join "-"
        )
        (Random.int 0 Random.maxInt)
        (Random.int 0 Random.maxInt)
        (Random.int 0 Random.maxInt)
        (Random.int 0 Random.maxInt)
