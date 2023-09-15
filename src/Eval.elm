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


--type alias ReplError =
--    { tipe : String
--    , title : String
--    , problems : List Problem
--    }
--
--replErrorCodec : Codec ReplData
--replErrorCodec =
--    Codec.object ReplError
--        |> Codec.field "name" .name (Codec.maybe Codec.string)
--        |> Codec.field "value" .value Codec.string
--        |> Codec.field "type" .tipe Codec.string
--        |> Codec.buildObject

--errorCodec : Codec ReplError

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

--foo = {\"type\":\"compile-errors\",
--       \"errors\":[{\"path\":\"/repl\"
--       ,\"name\":\"Elm_Repl\"
--       ,\"problems\":[{\"title\":\"TYPE MISMATCH\"
--       ,\"region\":{\"start\":{\"line\":3
--       ,\"column\":3},\"end\":{\"line\":3
--       ,\"column\":4}}
--       ,\"message\":[\"The (++) operator can append List and String values, but not \",{\"bold\":false,\"underline\":false,\"color\":\"yellow\",\"string\":\"number\"},\" values like\\nthis:\\n\\n3|   1++ 1\\n     \",{\"bold\":false,\"underline\":false,\"color\":\"RED\",\"string\":\"^\"},\"\\nTry using \",{\"bold\":false,\"underline\":false,\"color\":\"GREEN\",\"string\":\"String.fromInt\"},\" to turn it into a string? Or put it in [] to make it a\\nlist? Or switch to the (::) operator?\"]}]}]}