module Common.Text.Reduce exposing
    ( argIntoArg
    , argIntoMarked
    , markedIntoArg
    , textIntoArg
    , textIntoMarked
    )

import Common.Syntax exposing (Text(..))


argIntoArg : List Text -> List Text
argIntoArg stack =
    case stack of
        (Arg textList1 meta1) :: (Arg textList2 meta2) :: rest ->
            Arg (textList1 ++ textList2) { start = meta2.start, end = meta1.end, indent = 0, id = meta2.id } :: rest

        _ ->
            stack


argIntoMarked : List Text -> List Text
argIntoMarked stack =
    case stack of
        (Arg textList1 meta1) :: (Marked name textList2 meta2) :: rest ->
            Marked name (textList1 ++ textList2) { start = meta2.start, end = meta1.end, indent = 0, id = meta2.id } :: rest

        _ ->
            stack


markedIntoArg : List Text -> List Text
markedIntoArg stack =
    case stack of
        (Marked name textList1 meta1) :: (Arg textList2 meta2) :: rest ->
            Arg [ Marked name textList1 meta1 ] { start = meta2.start, end = meta1.end, indent = 0, id = meta2.id } :: rest

        _ ->
            stack


textIntoArg : List Text -> List Text
textIntoArg stack =
    case stack of
        (Text str meta1) :: (Arg textList2 meta2) :: rest ->
            Arg (Text str meta1 :: textList2) { start = meta2.start, end = meta1.end, indent = 0, id = meta2.id } :: rest

        _ ->
            stack


textIntoMarked : List Text -> List Text
textIntoMarked stack =
    case stack of
        (Text str meta1) :: (Marked name textList2 meta2) :: rest ->
            Marked name (Text str meta1 :: textList2) { start = meta2.start, end = meta1.end, indent = 0, id = meta2.id } :: rest

        _ ->
            stack
