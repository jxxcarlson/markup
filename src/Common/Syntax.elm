module Common.Syntax exposing
    ( Block(..)
    , BlockType(..)
    , Expr(..)
    , Language(..)
    , Meta
    , TextBlock(..)
    , dummyMeta
    , getName
    , isMarked
    , isPureText
    , isPureTextOrVerbatim
    , listSatisfies
    , map
    , textBlockToString
    , textToString
    )

import Bool.Extra



-- TYPES


type Block
    = Paragraph (List String) Meta
    | VerbatimBlock String (List String) Meta
    | Block String (List Block) Meta
    | BlockError String


type Expr
    = Text String Meta
    | Expr String (List Expr) Meta
    | Arg (List Expr) Meta
    | Verbatim String String Meta
    | ExprError String


getName : Expr -> Maybe String
getName text =
    case text of
        Expr str _ _ ->
            Just str

        _ ->
            Nothing


{-| -}
type Language
    = L1
    | Markdown
    | MiniLaTeX


type TextBlock
    = TBParagraph (List Expr) Meta
    | TBVerbatimBlock String (List String) Meta
    | TBBlock String (List TextBlock) Meta
    | TBError String


type alias Meta =
    { begin : Int
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
    { begin = start, end = start, indent = indent, id = "1.2" }



-- FUNCTIONS


map : (String -> List Expr) -> Block -> TextBlock
map textParser block =
    case block of
        Paragraph stringList meta ->
            TBParagraph (List.map textParser stringList |> List.concat) meta

        VerbatimBlock name stringList meta ->
            TBVerbatimBlock name stringList meta

        Block name blockList meta ->
            TBBlock name (List.map (map textParser) blockList) meta

        BlockError str ->
            TBError str


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


textToString : Expr -> String
textToString text =
    case text of
        Text string _ ->
            string

        Expr _ textList _ ->
            List.map textToString textList |> String.join "\n"

        Arg textList _ ->
            List.map textToString textList |> String.join "\n"

        Verbatim _ str _ ->
            str

        ExprError str ->
            str



-- PREDICATES


isMarked : Expr -> Bool
isMarked text =
    case text of
        Expr _ _ _ ->
            True

        _ ->
            False


isPureText : Expr -> Bool
isPureText text =
    case text of
        Text _ _ ->
            True

        _ ->
            False


isPureTextOrVerbatim : Expr -> Bool
isPureTextOrVerbatim text =
    case text of
        Text _ _ ->
            True

        Verbatim _ _ _ ->
            True

        _ ->
            False


listSatisfies : (Expr -> Bool) -> List Expr -> Bool
listSatisfies predicate textList =
    textList |> List.map predicate |> Bool.Extra.all



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
