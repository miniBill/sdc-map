module Theme exposing (column, padding, spacing)

import Ui exposing (Attribute, Element)


padding : Attribute msg
padding =
    Ui.padding rythm


spacing : Attribute msg
spacing =
    Ui.spacing rythm


rythm : number
rythm =
    10


column : List (Attribute msg) -> List (Element msg) -> Element msg
column attrs =
    Ui.column (spacing :: attrs)
