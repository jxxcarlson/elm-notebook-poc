module Eval exposing (..)

---( replDataCodec, submitExpression, encodeExpr, decodeReplError)

import Codec exposing (Codec, Value)
import Dict
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


submitExpression : EvalState -> String -> Cmd Msg
submitExpression evalState expr =
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
