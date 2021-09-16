module Common.Syntax exposing
    ( BasicBlock(..)
    , Block(..)
    , BlockType(..)
    , Meta
    , dummyMeta
    , simplify
    )


type BasicBlock
    = BBParagraph (List String)
    | BBVerbatimBlock String (List String)
    | BBBlock String (List BasicBlock)


simplify : Block -> BasicBlock
simplify block =
    case block of
        Paragraph strings _ ->
            BBParagraph strings

        VerbatimBlock name strings _ ->
            BBVerbatimBlock name strings

        Block name blocks _ ->
            BBBlock name (List.map simplify blocks)


type Block
    = Paragraph (List String) Meta
    | VerbatimBlock String (List String) Meta
    | Block String (List Block) Meta


type alias Meta =
    { start : Int
    , end : Int
    , indent : Int
    , id : String
    }


type BlockType
    = P
    | V
    | B


dummyMeta : Int -> Int -> Meta
dummyMeta start indent =
    { start = start, end = start, indent = indent, id = "76" }
