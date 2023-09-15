module Decode exposing (..)

import Json.Decode as D



type alias ReplError =
    { tipe : String
     , errors: List ErrorItem }

type alias ErrorItem =
    { path : String
    , name : String
    , problems : List Problem
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

type MessageItem = Plain String | Styled StyledString

type alias Region =
    { start : Position
    , end : Position
    }

type alias Position =
    { line : Int
    , column : Int
    }

