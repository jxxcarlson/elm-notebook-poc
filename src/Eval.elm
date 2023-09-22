module Eval exposing
    ( EvalState
    , bad
    , encodeExpr
    , hasReplError
    , initEvalState
    , initEvalStateX
    , replDataCodec
    , report
    , requestEvaluation
    )

import Codec exposing (Codec, Value)
import Dict
import ErrorReporter
import Http
import Json.Encode as Encode
import Types exposing (Msg(..), ReplData)


replDataCodec : Codec ReplData
replDataCodec =
    Codec.object ReplData
        |> Codec.field "name" .name (Codec.maybe Codec.string)
        |> Codec.field "value" .value Codec.string
        |> Codec.field "type" .tipe Codec.string
        |> Codec.buildObject


requestEvaluation : EvalState -> String -> Cmd Msg
requestEvaluation evalState expr =
    Http.post
        { url = "http://localhost:8000/repl"
        , body = Http.jsonBody (encodeExpr evalState expr)
        , expect = Http.expectString GotReply
        }


type alias EvalState =
    { decls : Dict.Dict String String
    , types : Dict.Dict String String
    , imports : Dict.Dict String String
    }


initEvalStateX : EvalState
initEvalStateX =
    { decls = Dict.fromList [ ( "foo", "1" ) ]
    , types = Dict.fromList [ ( "foo", "Int" ) ]
    , imports = Dict.empty
    }


initEvalState : EvalState
initEvalState =
    { decls = Dict.empty
    , types = Dict.empty
    , imports = Dict.empty
    }


encodeExpr : EvalState -> String -> Encode.Value
encodeExpr evalState expr =
    Encode.object
        [ ( "entry", Encode.string expr )
        , ( "imports", Encode.dict identity Encode.string evalState.imports )
        , ( "types", Encode.dict identity Encode.string evalState.types )
        , ( "decls", Encode.dict identity Encode.string evalState.decls )
        ]


report : String -> List ErrorReporter.MessageItem
report str =
    case ErrorReporter.decodeErrorReporter (str |> Debug.log "STR") of
        Ok replError ->
            renderReplError replError

        Err _ ->
            unknownReplError str


bad =
    "{\"type\":\"compile-errors\",\"errors\":[{\"path\":\"/repl\",\"name\":\"Elm_Repl\",\"problems\":[{\"title\":\"UNEXPECTED CAPITAL LETTER\",\"region\":{\"start\":{\"line\":2,\"column\":1},\"end\":{\"line\":2,\"column\":1}},\"message\":[\"Declarations always start with a lower-case letter, so I am getting stuck here:\\n\\n2| Int1repl_input_value_ =\\n \",{\"bold\":false,\"underline\":false,\"color\":\"RED\",\"string\":\"^\"},\"\\nTry a name like \",{\"bold\":false,\"underline\":false,\"color\":\"GREEN\",\"string\":\"int1repl_input_value_\"},\" instead?\\n\\n\",{\"bold\":false,\"underline\":true,\"color\":null,\"string\":\"Note\"},\": Here are a couple valid declarations for reference:\\n\\n greet : String -> String\\n greet name =\\n \",{\"bold\":false,\"underline\":false,\"color\":\"yellow\",\"string\":\"\\\"Hello \\\"\"},\" ++ name ++ \",{\"bold\":false,\"underline\":false,\"color\":\"yellow\",\"string\":\"\\\"!\\\"\"},\"\\n \\n \",{\"bold\":false,\"underline\":false,\"color\":\"CYAN\",\"string\":\"type\"},\" User = Anonymous | LoggedIn String\\n\\nNotice that they always start with a lower-case letter. Capitalization matters!\"]}]}]}"


hasReplError : String -> Bool
hasReplError str =
    String.left 24 str == "{\"type\":\"compile-errors\""


renderReplError : { a | errors : List { b | problems : List { c | message : List d } } } -> List d
renderReplError replError =
    replError
        |> .errors
        |> List.concatMap .problems
        |> List.concatMap .message


unknownReplError str =
    let
        _ =
            Debug.log "STR" str
    in
    [ ErrorReporter.Plain <| "Unknown REPL error: " ++ Debug.toString str ]
