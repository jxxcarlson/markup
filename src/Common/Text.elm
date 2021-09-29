module Common.Text exposing (combine, reverse, reverseMarked, stringValue, stringValueOfList)

import Common.Syntax exposing (Text(..))


stringValueOfList : List Text -> String
stringValueOfList textList =
    String.join " " (List.map stringValue textList)


stringValue : Text -> String
stringValue text =
    case text of
        Text str _ ->
            str

        Marked _ textList _ ->
            String.join " " (List.map stringValue textList)

        Arg textList _ ->
            String.join " " (List.map stringValue textList)

        TError str ->
            str

        Verbatim _ str _ ->
            str


reverse : Text -> Text
reverse text =
    case text of
        Marked name textList meta ->
            Marked name (List.reverse textList) meta

        Arg textList meta ->
            Arg (List.reverse textList) meta

        _ ->
            text


reverseMarked : Text -> Text
reverseMarked text =
    case text of
        Marked name textList meta ->
            Marked name (List.reverse textList) meta

        _ ->
            text


combine : List Text -> List Text
combine textList =
    case textList of
        text1 :: text2 :: rest ->
            case ( text1, text2 ) of
                ( Text str1 meta1, Text str2 meta2 ) ->
                    combine (Text (str1 ++ str2) { start = meta1.start, end = meta2.end, id = meta1.id, indent = meta1.indent } :: rest)

                _ ->
                    textList

        _ ->
            textList
