port module PkgPorts exposing (decrypt, decrypted, encrypt, encrypted)


port encrypt : { input : String, serverPublic : String } -> Cmd msg


port encrypted : (String -> msg) -> Sub msg


port decrypt : { inputs : List String, serverSecret : String } -> Cmd msg


port decrypted : (List String -> msg) -> Sub msg
