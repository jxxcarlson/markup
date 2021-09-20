module Common.Syntax exposing
    ( Block(..)
    , BlockType(..)
    , Meta
    , Text(..)
    , TextBlock(..)
    , dummyMeta
    , map
    , mapList
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
    = Text (List String) Meta
    | Marked String (List Text) Meta
    | Arg (List Text) Meta
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
    { start = start, end = start, indent = indent, id = "1.2" }



-- FUNCTIONS


mapList : (List String -> List Text) -> Block -> TextBlock
mapList f block =
    case block of
        Paragraph stringList meta ->
            TBParagraph (f stringList) meta

        VerbatimBlock name stringList meta ->
            TBVerbatimBlock name (f stringList) meta

        Block name blockList meta ->
            TBBlock name (List.map (mapList f) blockList) meta

        Error str ->
            TBError str


map : (String -> Text) -> Block -> TextBlock
map f block =
    case block of
        Paragraph stringList meta ->
            TBParagraph (List.map f stringList) meta

        VerbatimBlock name stringList meta ->
            TBVerbatimBlock name (List.map f stringList) meta

        Block name blockList meta ->
            TBBlock name (List.map (map f) blockList) meta

        Error str ->
            TBError str


textBlockToString : TextBlock -> List String
textBlockToString textBlock =
    case textBlock of
        TBParagraph textList _ ->
            List.map textToString textList

        TBVerbatimBlock _ textList _ ->
            List.map textToString textList

        TBBlock _ textBlockList _ ->
            List.map (textBlockToString >> String.join "\n") textBlockList

        TBError str ->
            [ str ]


textToString : Text -> String
textToString text =
    case text of
        Text stringlist _ ->
            Utility.prepare stringlist |> String.join "\n"

        Marked _ textList _ ->
            List.map textToString textList |> String.join "\n"

        Arg textList _ ->
            List.map textToString textList |> String.join "\n"

        Verbatim _ textList _ ->
            List.map textToString textList |> String.join "\n"

        TError str ->
            str
