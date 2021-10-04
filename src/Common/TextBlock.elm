module Common.TextBlock exposing (stringValue)

import Common.Syntax exposing (Expr(..), TextBlock(..))
import Common.Text


stringValue : String -> TextBlock -> String
stringValue joiner block =
    case block of
        TBParagraph textList _ ->
            String.join joiner (List.map Common.Text.stringValue textList)

        TBVerbatimBlock _ strList _ ->
            String.join joiner strList

        TBBlock _ blockList _ ->
            String.join joiner (List.map (stringValue joiner) blockList)

        TBError str ->
            str
