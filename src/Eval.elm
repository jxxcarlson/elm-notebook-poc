module Eval exposing
    ( EvalState
    , encodeExpr
    , hasReplError
    , initEvalState
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



--initEvalState : EvalState
--initEvalState =
--    { decls = Dict.fromList [ ( "greet", """greet : String -> String greet name = "Hello " ++ name""" ) ]
--    , types = Dict.empty
--    , imports = Dict.empty
--    }


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
            renderReplReplError replError

        Err _ ->
            unknownReplError str


hasReplError : String -> Bool
hasReplError str =
    String.left 24 str == "{\"type\":\"compile-errors\""


renderReplReplError replError =
    replError
        |> .errors
        |> List.concatMap .problems
        |> List.concatMap .message


unknownReplError str =
    let
        _ =
            Debug.log "STR" str
    in
    [ ErrorReporter.Plain "Oops, something wen't wrong." ]
