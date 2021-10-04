module Common.Text exposing (combine, reverse, reverseMarked, stringValue, stringValueOfList)

import Common.Syntax exposing (Expr(..))


stringValueOfList : List Expr -> String
stringValueOfList textList =
    String.join " " (List.map stringValue textList)


stringValue : Expr -> String
stringValue text =
    case text of
        Text str _ ->
            str

        Expr _ textList _ ->
            String.join " " (List.map stringValue textList)

        Arg textList _ ->
            String.join " " (List.map stringValue textList)

        ExprError str ->
            str

        Verbatim _ str _ ->
            str


reverse : Expr -> Expr
reverse text =
    case text of
        Expr name textList meta ->
            Expr name (List.reverse textList) meta

        Arg textList meta ->
            Arg (List.reverse textList) meta

        _ ->
            text


reverseMarked : Expr -> Expr
reverseMarked text =
    case text of
        Expr name textList meta ->
            Expr name (List.reverse textList) meta

        _ ->
            text


combine : List Expr -> List Expr
combine textList =
    case textList of
        text1 :: text2 :: rest ->
            case ( text1, text2 ) of
                ( Text str1 meta1, Text str2 meta2 ) ->
                    combine (Text (str1 ++ str2) { begin = meta1.begin, end = meta2.end, id = meta1.id, indent = meta1.indent } :: rest)

                _ ->
                    textList

        _ ->
            textList
