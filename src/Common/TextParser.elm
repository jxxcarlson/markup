module Common.TextParser exposing (..)

import Common.Syntax exposing (Block(..), Meta, Text(..), TextBlock(..), dummyMeta)


parseText : List String -> Meta -> List Text
parseText strings meta =
    [ Text strings meta ]


parseTextInBlock : Block -> TextBlock
parseTextInBlock block =
    case block of
        Paragraph strings meta ->
            TBParagraph (parseText strings meta) meta

        VerbatimBlock name strings meta ->
            TBVerbatimBlock name (parseText strings meta) meta

        Block name blocks meta ->
            TBBlock name (List.map parseTextInBlock blocks) meta

        Error e ->
            TBError e
