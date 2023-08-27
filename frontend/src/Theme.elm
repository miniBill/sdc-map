module Theme exposing (column, padding, rythm, spacing)

import Element exposing (Attribute, Element)


padding : Attribute msg
padding =
    Element.padding rythm


spacing : Attribute msg
spacing =
    Element.spacing rythm


rythm : number
rythm =
    10


column : List (Attribute msg) -> List (Element msg) -> Element msg
column attrs =
    Element.column (spacing :: attrs)
