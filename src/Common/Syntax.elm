module Common.Syntax exposing
    ( Block(..)
    , BlockM
    , BlockType(..)
    , dummyMeta
    )


type Block
    = Paragraph (List String)
    | VerbatimBlock String (List String)
    | Block String (List Block)


type BlockType
    = P
    | V
    | B


type alias BlockM =
    { content : Block, meta : Maybe Meta }


type alias Meta =
    { start : Int
    , end : Int
    , indent : Int
    , id : String
    }


dummyMeta : Int -> Int -> Meta
dummyMeta start indent =
    { start = start, end = start, indent = indent, id = "76" }
