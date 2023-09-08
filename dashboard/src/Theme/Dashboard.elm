module Theme.Dashboard exposing (Column, cell, header, table, tableCell, tableColumnElement, tableColumnNumber, tableColumnText)

import Element exposing (Attribute, Element, Length, alignRight, centerY, el, shrink, text)
import Element.Border as Border
import Element.Font as Font
import Theme exposing (rythm)


type alias Column element msg =
    { header : String
    , view : element -> ( List (Attribute msg), Element msg )
    , width : Length
    }


tableColumnElement : String -> (element -> Element msg) -> Column element msg
tableColumnElement headerLabel toCell =
    { header = headerLabel
    , view = \record -> ( [], toCell record )
    , width = shrink
    }


tableColumnNumber : String -> (element -> Int) -> Column element msg
tableColumnNumber headerLabel toCell =
    { header = headerLabel
    , view = \record -> ( [ alignRight ], text <| String.fromInt <| toCell record )
    , width = shrink
    }


tableColumnText : String -> (element -> String) -> Column element msg
tableColumnText headerLabel toCell =
    tableColumnElement headerLabel
        (\record -> text <| toCell record)


table :
    List (Attribute msg)
    ->
        { data : List record
        , columns : List (Column record msg)
        }
    -> Element msg
table attrs config =
    Element.indexedTable attrs
        { data = config.data
        , columns =
            config.columns
                |> List.indexedMap
                    (\colIndex columnDefinition ->
                        { header = header colIndex columnDefinition.header
                        , view = tableCell colIndex columnDefinition.view
                        , width = shrink
                        }
                    )
        }


cell : List (Attribute msg) -> Element msg -> ( List (Attribute msg), Element msg )
cell attrs child =
    ( attrs, child )


tableCell : Int -> (record -> ( List (Attribute msg), Element msg )) -> Int -> record -> Element msg
tableCell colIndex cellView rowIndex record =
    let
        leftPadding : number
        leftPadding =
            if colIndex == 0 then
                0

            else
                rythm

        topPadding : Int
        topPadding =
            if rowIndex == 0 then
                rythm // 2

            else
                rythm

        ( attrs, child ) =
            cellView record
    in
    el
        ([ Element.paddingEach
            { top = topPadding
            , left = leftPadding
            , right = 0
            , bottom = 0
            }
         , centerY
         ]
            ++ attrs
        )
        child


header : Int -> String -> Element msg
header colIndex child =
    let
        leftPadding : number
        leftPadding =
            if colIndex == 0 then
                0

            else
                rythm
    in
    el
        [ Font.bold
        , Border.widthEach
            { top = 0
            , left = 0
            , right = 0
            , bottom = 1
            }
        , Element.paddingEach
            { top = 0
            , left = leftPadding
            , right = 0
            , bottom = 0
            }
        ]
        (text child)
