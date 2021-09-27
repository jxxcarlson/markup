module Common.Text exposing (..)

import Common.Syntax exposing (Text(..))


reverse : Text -> Text
reverse text =
    case text of
        Marked name textList meta ->
            Marked name (List.reverse textList) meta

        Arg textList meta ->
            Arg (List.reverse textList) meta

        _ ->
            text
