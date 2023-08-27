port module PkgPorts exposing (encrypt, encrypted)


port encrypt : String -> Cmd msg


port encrypted : (String -> msg) -> Sub msg
