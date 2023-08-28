module Generate exposing (main)

{-| -}

import Elm
import Gen.CodeGen.Generate as Generate
import Gen.Dict
import ISO3166


main : Program {} () ()
main =
    Generate.run [ file ]


file : Elm.File
file =
    ISO3166.all
        |> List.map
            (\{ name, subdivisions } ->
                Elm.tuple
                    (name
                        |> String.replace "United Kingdom of Great Britain and Northern Ireland" "UK"
                        |> String.replace "United States of America" "USA"
                        |> Elm.string
                    )
                    (Elm.list <| List.map (Elm.string << .name) subdivisions)
            )
        |> Gen.Dict.fromList
        |> Elm.declaration "subdivisions"
        |> Elm.expose
        |> List.singleton
        |> Elm.file [ "Subdivisions" ]
