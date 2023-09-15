module Eval exposing ( replDataCodec, submitExpression, encodeExpr)

import Dict
import Json.Encode as Encode
import Codec exposing (Codec, Value)
import Http
import Types exposing (Msg(..), ReplData)




replDataCodec : Codec ReplData
replDataCodec =
    Codec.object ReplData
        |> Codec.field "name" .name (Codec.maybe Codec.string)
        |> Codec.field "value" .value Codec.string
        |> Codec.field "type" .tipe Codec.string
        |> Codec.buildObject

submitExpression : String -> Cmd Msg
submitExpression expr =
  Http.post
    { url = "http://localhost:8000/repl"
    , body = Http.jsonBody (encodeExpr expr)
    , expect = Http.expectString GotReply
    }


encodeExpr : String -> Encode.Value
encodeExpr expr =
    Encode.object
        [ ( "entry", Encode.string expr )
        , ( "imports", Encode.dict identity identity Dict.empty )
        , ( "types", Encode.dict identity identity Dict.empty )
        , ( "decls", Encode.dict identity identity Dict.empty )
        ]