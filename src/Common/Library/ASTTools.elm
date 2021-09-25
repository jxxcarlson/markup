module Common.Library.ASTTools exposing (filter, getHeadings, getTitle)

import Common.Syntax as Syntax


getTitle : List Syntax.TextBlock -> Maybe String
getTitle blocks =
    filterStrict "title" blocks |> List.head |> Maybe.map (Syntax.textToString >> String.trim)


getHeadings : List Syntax.TextBlock -> List Syntax.Text
getHeadings blocks =
    filter "heading" blocks


filter : String -> List Syntax.TextBlock -> List Syntax.Text
filter key blocks =
    List.map (filter_ key) blocks |> List.concat


filterStrict : String -> List Syntax.TextBlock -> List Syntax.Text
filterStrict key blocks =
    List.map (filterStrict_ key) blocks |> List.concat


filterStrictNot : String -> List Syntax.TextBlock -> List Syntax.Text
filterStrictNot key blocks =
    List.map (filterStrictNot_ key) blocks |> List.concat


filter_ : String -> Syntax.TextBlock -> List Syntax.Text
filter_ key block =
    case block of
        Syntax.TBParagraph textList _ ->
            List.filter (\t -> Maybe.map (String.contains key) (Syntax.getName t) == Just True) textList

        Syntax.TBBlock _ blocks _ ->
            List.map (filter_ key) blocks |> List.concat

        _ ->
            []


filterStrict_ : String -> Syntax.TextBlock -> List Syntax.Text
filterStrict_ key block =
    case block of
        Syntax.TBParagraph textList _ ->
            List.filter (\t -> Just key == Syntax.getName t) textList

        Syntax.TBBlock _ blocks _ ->
            List.map (filterStrict_ key) blocks |> List.concat

        _ ->
            []


filterStrictNot_ : String -> Syntax.TextBlock -> List Syntax.Text
filterStrictNot_ key block =
    case block of
        Syntax.TBParagraph textList _ ->
            List.filter (\t -> Just key /= Syntax.getName t) textList

        Syntax.TBBlock _ blocks _ ->
            List.map (filterStrict_ key) blocks |> List.concat

        _ ->
            []
