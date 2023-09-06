module Evergreen.Migrate.V11 exposing (..)

{-| This migration file was automatically generated by the lamdera compiler.

It includes:

  - A migration for each of the 6 Lamdera core types that has changed
  - A function named `migrate_ModuleName_TypeName` for each changed/custom type

Expect to see:

  - `Unimplementеd` values as placeholders wherever I was unable to figure out a clear migration path for you
  - `@NOTICE` comments for things you should know about, i.e. new custom type constructors that won't get any
    value mappings from the old type by default

You can edit this file however you wish! It won't be generated again.

See <https://dashboard.lamdera.app/docs/evergreen> for more info.

-}

import Evergreen.V11.Types
import Evergreen.V4.Types
import Lamdera.Migrations exposing (..)
import Dict exposing (Dict)


frontendModel : Evergreen.V4.Types.FrontendModel -> ModelMigration Evergreen.V11.Types.FrontendModel Evergreen.V11.Types.FrontendMsg
frontendModel old =
    ModelMigrated ( migrate_Types_FrontendModel old, Cmd.none )


backendModel : Evergreen.V4.Types.BackendModel -> ModelMigration Evergreen.V11.Types.BackendModel Evergreen.V11.Types.BackendMsg
backendModel old =
    ModelUnchanged


frontendMsg : Evergreen.V4.Types.FrontendMsg -> MsgMigration Evergreen.V11.Types.FrontendMsg Evergreen.V11.Types.FrontendMsg
frontendMsg old =
    MsgMigrated ( migrate_Types_FrontendMsg old, Cmd.none )


toBackend : Evergreen.V4.Types.ToBackend -> MsgMigration Evergreen.V11.Types.ToBackend Evergreen.V11.Types.BackendMsg
toBackend old =
    MsgUnchanged


backendMsg : Evergreen.V4.Types.BackendMsg -> MsgMigration Evergreen.V11.Types.BackendMsg Evergreen.V11.Types.BackendMsg
backendMsg old =
    MsgUnchanged


toFrontend : Evergreen.V4.Types.ToFrontend -> MsgMigration Evergreen.V11.Types.ToFrontend Evergreen.V11.Types.FrontendMsg
toFrontend old =
    MsgUnchanged


migrate_Types_EncryptedString : Evergreen.V4.Types.EncryptedString -> Evergreen.V11.Types.EncryptedString
migrate_Types_EncryptedString old =
    case old of
        Evergreen.V4.Types.EncryptedString p0 ->
            Evergreen.V11.Types.EncryptedString p0


migrate_Types_FrontendModel : Evergreen.V4.Types.FrontendModel -> Evergreen.V11.Types.FrontendModel
migrate_Types_FrontendModel old =
    case old of
        Evergreen.V4.Types.Filling p0 p1 ->
            Evergreen.V11.Types.Filling (p0 |> migrate_Types_Input) p1

        Evergreen.V4.Types.Encrypting p0 ->
            Evergreen.V11.Types.Encrypting (p0 |> migrate_Types_Input)

        Evergreen.V4.Types.Submitting p0 ->
            Evergreen.V11.Types.Submitting (p0 |> migrate_Types_Input)

        Evergreen.V4.Types.Submitted p0 ->
            Evergreen.V11.Types.Submitted
                { id = p0.id
                , input = p0.input |> migrate_Types_Input
                }

        Evergreen.V4.Types.AdminDecrypting p0 p1 ->
            Evergreen.V11.Types.AdminDecrypting p0
                (p1 |> Dict.map (\k -> migrate_Types_EncryptedString))

        Evergreen.V4.Types.AdminDecrypted _ p1 ->
            Evergreen.V11.Types.AdminDecrypted p1


migrate_Types_FrontendMsg : Evergreen.V4.Types.FrontendMsg -> Evergreen.V11.Types.FrontendMsg
migrate_Types_FrontendMsg old =
    case old of
        Evergreen.V4.Types.Name p0 ->
            Evergreen.V11.Types.Name p0

        Evergreen.V4.Types.Country p0 ->
            Evergreen.V11.Types.Country p0

        Evergreen.V4.Types.Location p0 ->
            Evergreen.V11.Types.Location p0

        Evergreen.V4.Types.NameOnMap p0 ->
            Evergreen.V11.Types.NameOnMap p0

        Evergreen.V4.Types.Id p0 ->
            Evergreen.V11.Types.Id p0

        Evergreen.V4.Types.Captcha p0 ->
            Evergreen.V11.Types.Captcha p0

        Evergreen.V4.Types.Submit ->
            Evergreen.V11.Types.Submit

        Evergreen.V4.Types.Encrypted p0 ->
            Evergreen.V11.Types.Encrypted (p0 |> migrate_Types_EncryptedString)

        Evergreen.V4.Types.AdminSecretKey p0 ->
            Evergreen.V11.Types.AdminSecretKey p0

        Evergreen.V4.Types.Decrypt ->
            Evergreen.V11.Types.Decrypt

        Evergreen.V4.Types.Decrypted p0 ->
            Evergreen.V11.Types.Decrypted p0

        Evergreen.V4.Types.CaptchaIsValid p0 p1 ->
            Evergreen.V11.Types.Nop

        Evergreen.V4.Types.Nop ->
            Evergreen.V11.Types.Nop


migrate_Types_Input : Evergreen.V4.Types.Input -> Evergreen.V11.Types.Input
migrate_Types_Input old =
    old
