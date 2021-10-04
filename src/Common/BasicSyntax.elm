module Common.BasicSyntax exposing (..)

import Common.Syntax exposing (Block(..), Expr, TextBlock(..))


type BasicBlock
    = BBParagraph (List String)
    | BBVerbatimBlock String (List String)
    | BBBlock String (List BasicBlock)
    | BBError String


type BasicTextBlock
    = TBBParagraph (List Expr)
    | TBBVerbatimBlock String (List String)
    | TBBBlock String (List BasicTextBlock)
    | TBBError String


simplifyText : TextBlock -> BasicTextBlock
simplifyText block =
    case block of
        TBParagraph texts _ ->
            TBBParagraph texts

        TBVerbatimBlock name texts _ ->
            TBBVerbatimBlock name texts

        TBBlock name blocks _ ->
            TBBBlock name (List.map simplifyText blocks)

        TBError desc ->
            TBBError desc


simplify : Block -> BasicBlock
simplify block =
    case block of
        Paragraph strings _ ->
            BBParagraph strings

        VerbatimBlock name strings _ ->
            BBVerbatimBlock name strings

        Block name blocks _ ->
            BBBlock name (List.map simplify blocks)

        BlockError desc ->
            BBError desc
