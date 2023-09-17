module ErrorReporter exposing (MessageItem(..), StyledString, decodeErrorReporter, renderMessageItem, stringToMessageItem)

{-| This module contains the decoders for the error messages that the repl
-}

import Color
import Element exposing (..)
import Element.Font as Font
import Json.Decode as D


type alias ReplError =
    { tipe : String
    , errors : List ErrorItem
    }


type alias ErrorItem =
    { path : String
    , name : String
    , problems : List Problem
    }


type alias Region =
    { start : Position
    , end : Position
    }


type alias Position =
    { line : Int
    , column : Int
    }


type alias Problem =
    { title : String
    , region : Region
    , message : List MessageItem
    }


type alias StyledString =
    { bold : Bool
    , underline : Bool
    , color : String
    , string : String
    }


renderMessageItem : MessageItem -> Element msg
renderMessageItem messageItem =
    case messageItem of
        Plain str ->
            el [] (text (str |> Debug.log "PLAIN_STRING"))

        Styled styledString ->
            let
                color =
                    case styledString.color of
                        "red" ->
                            Element.rgb 1 0 0

                        "green" ->
                            Element.rgb 0 1 0

                        "blue" ->
                            Element.rgb 0 0 1

                        "yellow" ->
                            Element.rgb 1 1 0

                        "black" ->
                            Element.rgb 0.9 0.4 0.1

                        "white" ->
                            Element.rgb 1 1 1

                        _ ->
                            Element.rgb 0.9 0.4 0.1

                style =
                    if styledString.bold then
                        Font.bold

                    else if styledString.underline then
                        Font.underline

                    else
                        Font.unitalicized
            in
            el [ paddingXY 8 8, Font.color color, style ] (text (styledString.string |> Debug.log "STY_STRING"))


stringToMessageItem : String -> MessageItem
stringToMessageItem str =
    Plain str


type MessageItem
    = Plain String
    | Styled StyledString



-- decodeErrorReporter : String -> Result String ReplError


decodeErrorReporter str =
    D.decodeString replErrorDecoder str


replErrorDecoder : D.Decoder ReplError
replErrorDecoder =
    D.map2 ReplError
        (D.field "type" D.string)
        (D.field "errors" (D.list errorItemDecoder))


errorItemDecoder : D.Decoder ErrorItem
errorItemDecoder =
    D.map3 ErrorItem
        (D.field "path" D.string)
        (D.field "name" D.string)
        (D.field "problems" (D.list problemDecoder))


problemDecoder : D.Decoder Problem
problemDecoder =
    D.map3 Problem
        (D.field "title" D.string)
        (D.field "region" regionDecoder)
        (D.field "message" (D.list messageItemDecoder))


regionDecoder : D.Decoder Region
regionDecoder =
    D.map2 Region
        (D.field "start" positionDecoder)
        (D.field "end" positionDecoder)


positionDecoder : D.Decoder Position
positionDecoder =
    D.map2 Position
        (D.field "line" D.int)
        (D.field "column" D.int)


messageItemDecoder : D.Decoder MessageItem
messageItemDecoder =
    D.oneOf
        [ D.map Plain D.string
        , D.map Styled styledStringDecoder
        ]


styledStringDecoder : D.Decoder StyledString
styledStringDecoder =
    D.map4 StyledString
        (D.field "bold" D.bool)
        (D.field "underline" D.bool)
        (D.field "color" D.string)
        (D.field "string" D.string)


dec decoder str =
    D.decodeString decoder str



-- X1
--> x1N = """[{\"bold\":false,\"underline\":false,\"color\":\"GREEN\",\"string\":\"String.fromInt 77"}, "ho ho ho!"]"""
--"[{\"bold\":false,\"underline\":false,\"color\":\"GREEN\",\"string\":\"String.fromInt 77\"}, \"ho ho ho!\"]"
--    : String
--> dec (D.list messageItemDecoder) x1N
--  Ok [Styled { bold = False, color = "GREEN", string = "String.fromInt 77", underline = False },Plain ("ho ho ho!")]
-- X3
--> x3 = """{"title": "TITLE", "region":{"start":{"line":3,"column":3},"end":{"line":3,"column":4}},"message": [{"bold":false,"underline":false,"color":"GREEN","string":"String.fromInt 77"}, "ho ho ho!"]}"""
--> dec (problemDecoder) x2
--Ok { message = [Styled { bold = False, color = "GREEN", string = "String.fromInt 77", underline = False },Plain ("ho ho ho!")], region = { end = { column = 4, line = 3 }, start = { column = 3, line = 3 } }, title = "TITLE" }
-- X3
--> x3 = """{"path": "/repl", "name": "Elm_Repl", "problems":[""" ++ x2 ++ """]}"""
--"{\"path\": \"/repl\", \"name\": \"Elm_Repl\", \"problems\":[{\"title\": \"TITLE\", \"region\":{\"start\":{\"line\":3,\"column\":3},\"end\":{\"line\":3,\"column\":4}},\"message\": [{\"bold\":false,\"underline\":false,\"color\":\"GREEN\",\"string\":\"String.fromInt 77\"}, \"ho ho ho!\"]}]}"
--    : String
--> dec errorItemDecoder x3
--Ok { name = "Elm_Repl", path = "/repl", problems = [{ message = [Styled { bold = False, color = "GREEN", string = "String.fromInt 77", underline = False },Plain ("ho ho ho!")], region = { end = { column = 4, line = 3 }, start = { column = 3, line = 3 } }, title = "TITLE" }] }
--    : Result Error ErrorItem
-- X4:
--> x4 = """{ "type": "Compile Errors", "errors": [""" ++ x3 ++ """]}"""
--"{ \"type\": \"Compile Errors\", \"errors\": [{\"path\": \"/repl\", \"name\": \"Elm_Repl\", \"problems\":[{\"title\": \"TITLE\", \"region\":{\"start\":{\"line\":3,\"column\":3},\"end\":{\"line\":3,\"column\":4}},\"message\": [{\"bold\":false,\"underline\":false,\"color\":\"GREEN\",\"string\":\"String.fromInt 77\"}, \"ho ho ho!\"]}]}]}"
--    : String
--> dec replErrorDecoder x4
--Ok { errors = [{ name = "Elm_Repl", path = "/repl", problems = [{ message = [Styled { bold = False, color = "GREEN", string = "String.fromInt 77", underline = False },Plain ("ho ho ho!")], region = { end = { column = 4, line = 3 }, start = { column = 3, line = 3 } }, title = "TITLE" }] }], tipe = "Compile Errors" }
--    : Result Error ReplError
