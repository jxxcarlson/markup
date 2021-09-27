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



{-
   JUST FOR THINKING ABOUT L1
   NOTE: type L1Element maps into type Text
   [i Italic] => Marked "i" [Text "Italic"] meta
   [b [i Italic]] => Marked "b" [Marked "i" [Text "Italic"]]
   Etc

   Reductions:
   Text "i" :: Marked "" [] :: rest =>  Marked "i" []::rest

   [i Italic]
   01      23
   ----------
   0, Shift: [Marked "" []]
   1, Shift : [Text "i" , Marked " []]
   -, Reduce : [Marked "i" []]
   2, Shift : [Text "Italic", Marked "i" []]
   3, Reduce [], Marked "i" [Text "Italic"]]


    type L1Element
        = Text String Meta
        | L1Element String (List L1Element) Meta
        | Verbatim VerbatimType String Meta
        | Problem (List ParseError) String


    type VerbatimType
        = Code
        | Math
        | Quoted


    type ParseError
        = ErrorA
        | ErrorB


    type L1VerbatimType
        = VCode
        | VMath

-}


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
