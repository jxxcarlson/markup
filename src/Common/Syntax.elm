module Common.Syntax exposing
    ( Block(..)
    , BlockType(..)
    , Meta
    , Text(..)
    , TextBlock(..)
    , dummyMeta
    )


type Block
    = Paragraph (List String) Meta
    | VerbatimBlock String (List String) Meta
    | Block String (List Block) Meta
    | Error String


type Text
    = Text (List String) Meta
    | Marked String (List Text) Meta
    | Verbatim String (List Text) Meta
    | TError String


type TextBlock
    = TBParagraph (List Text) Meta
    | TBVerbatimBlock String (List Text) Meta
    | TBBlock String (List TextBlock) Meta
    | TBError String


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
    | E


dummyMeta : Int -> Int -> Meta
dummyMeta start indent =
    { start = start, end = start, indent = indent, id = "76" }
