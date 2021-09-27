module Common.Text.Reduce exposing
    ( argIntoArg
    , argIntoMarked
    , contract3Stack
    , contractStackRepeatedly
    , markedIntoArg
    , textIntoArg
    , textIntoMarked
    )

import Common.Syntax exposing (Text(..))


contract2 : Text -> Text -> Maybe Text
contract2 text1 text2 =
    case ( text1, text2 ) of
        ( Arg textList1 meta1, Arg textList2 meta2 ) ->
            Just <| Arg (textList1 ++ textList2) { start = meta2.start, end = meta1.end, indent = 0, id = meta2.id }

        ( Arg textList1 meta1, Marked name textList2 meta2 ) ->
            Just <| Marked name (textList1 ++ textList2) { start = meta2.start, end = meta1.end, indent = 0, id = meta2.id }

        ( Text str meta1, Arg textList2 meta2 ) ->
            Just <| Arg (Text str meta1 :: textList2) { start = meta2.start, end = meta1.end, indent = 0, id = meta2.id }

        ( Marked name textList1 meta1, Arg textList2 meta2 ) ->
            Just <| Arg (Marked name textList1 meta1 :: textList2) { start = meta2.start, end = meta1.end, indent = 0, id = meta2.id }

        ( Text str meta1, Marked name textList2 meta2 ) ->
            Just <| Marked name (Text str meta1 :: textList2) { start = meta2.start, end = meta1.end, indent = 0, id = meta2.id }

        ( _, _ ) ->
            Nothing


contract3 : Text -> Text -> Text -> Maybe Text
contract3 text1 text2 text3 =
    case ( text1, text2, text3 ) of
        ( Marked a [] meta1, _, Marked b [] meta3 ) ->
            if a == b then
                Just <| Marked a [ text2 ] { start = meta1.start, end = meta3.end, indent = 0, id = meta3.id }

            else
                Nothing

        ( Verbatim a "" meta1, Text x meta, Verbatim b "" meta3 ) ->
            if a == b then
                Just <| Verbatim a x { start = meta1.start, end = meta3.end, indent = 0, id = meta3.id }

            else
                Nothing

        _ ->
            Nothing


contract3Stack : List Text -> List Text
contract3Stack stack =
    case stack of
        text1 :: text2 :: text3 :: rest ->
            case contract3 text1 text2 text3 of
                Nothing ->
                    stack

                Just text_ ->
                    text_ :: rest

        _ ->
            stack


{-|

    Used in two places:

    - nextCursor, when there is no text left to process. This is natural
    - nextCursor_, in case Commit (this is also natural)

-}
contractStackRepeatedly : List Text -> List Text
contractStackRepeatedly stack =
    (case stack of
        text1 :: text2 :: text3 :: rest ->
            case contract3 text1 text2 text3 of
                Nothing ->
                    stack

                Just text_ ->
                    text_ :: rest

        text1 :: text2 :: rest ->
            case contract2 text1 text2 of
                Nothing ->
                    stack

                Just text_ ->
                    contractStackRepeatedly (text_ :: rest)

        _ ->
            stack
    )
        |> List.reverse


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
