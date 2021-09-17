module Common.TextParser exposing (parse)

import Common.Render
import Common.Syntax exposing (Block(..), Meta, Text(..), TextBlock(..), dummyMeta)


parse : Int -> Common.Render.Settings -> List String -> List Text
parse generation settings strings =
    [ Text strings (dummyMeta 0 0) ]


parseTextInBlock : Int -> Common.Render.Settings -> Block -> TextBlock
parseTextInBlock generation settings block =
    case block of
        Paragraph strings meta ->
            TBParagraph (parse generation settings strings) meta

        VerbatimBlock name strings meta ->
            TBVerbatimBlock name (parse generation settings strings) meta

        Block name blocks meta ->
            TBBlock name (List.map (parseTextInBlock generation settings) blocks) meta

        Error e ->
            TBError e
