module Common.Syntax exposing
    ( Block(..)
    , BlockType(..)
    , Language(..)
    , Meta
    , Text(..)
    , TextBlock(..)
    , dummyMeta
    , getName
    , map
    , textBlockToString
    , textToString
    )

import Utility



-- TYPES


type Block
    = Paragraph (List String) Meta
    | VerbatimBlock String (List String) Meta
    | Block String (List Block) Meta
    | Error String


type Text
    = Text String Meta
    | Marked String (List Text) Meta
    | Arg (List Text) Meta
    | Verbatim String String Meta
    | TError String


getName : Text -> Maybe String
getName text =
    case text of
        Marked str _ _ ->
            Just str

        _ ->
            Nothing


{-| -}
type Language
    = L1
    | Markdown
    | MiniLaTeX


type TextBlock
    = TBParagraph (List Text) Meta
    | TBVerbatimBlock String (List String) Meta
    | TBBlock String (List TextBlock) Meta
    | TBError String


map : (String -> List Text) -> Block -> TextBlock
map textParser block =
    case block of
        Paragraph stringList meta ->
            TBParagraph (List.map textParser stringList |> List.concat) meta

        VerbatimBlock name stringList meta ->
            TBVerbatimBlock name stringList meta

        Block name blockList meta ->
            TBBlock name (List.map (map textParser) blockList) meta

        Error str ->
            TBError str


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
    { start = start, end = start, indent = indent, id = "1.2" }



-- FUNCTIONS


textBlockToString : TextBlock -> List String
textBlockToString textBlock =
    case textBlock of
        TBParagraph textList _ ->
            List.map textToString textList

        TBVerbatimBlock _ textList _ ->
            textList

        TBBlock _ textBlockList _ ->
            List.map (textBlockToString >> String.join "\n") textBlockList

        TBError str ->
            [ str ]


textToString : Text -> String
textToString text =
    case text of
        Text string _ ->
            string

        Marked _ textList _ ->
            List.map textToString textList |> String.join "\n"

        Arg textList _ ->
            List.map textToString textList |> String.join "\n"

        Verbatim _ str _ ->
            str

        TError str ->
            str
