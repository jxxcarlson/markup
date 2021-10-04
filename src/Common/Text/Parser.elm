module Common.Text.Parser exposing (TextParser, dummyParse, parseTextInBlock)

import Common.Render.TextBlock
import Common.Syntax exposing (Block(..), Expr(..), Meta, TextBlock(..), dummyMeta)


type alias TextParser =
    Int -> Common.Render.TextBlock.Settings -> String -> List Expr


dummyParse : TextParser
dummyParse generation settings string =
    [ Text string (dummyMeta generation 0) ]


parseTextInBlock : Int -> Common.Render.TextBlock.Settings -> TextParser -> Block -> TextBlock
parseTextInBlock generation settings parse_ block =
    case block of
        Paragraph strings meta ->
            TBParagraph (parse_ generation settings (String.join "\n" strings)) meta

        VerbatimBlock name strings meta ->
            TBVerbatimBlock name strings meta

        Block name blocks meta ->
            TBBlock name (List.map (parseTextInBlock generation settings parse_) blocks) meta

        BlockError e ->
            TBError e
