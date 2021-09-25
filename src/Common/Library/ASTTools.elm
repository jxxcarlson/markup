module Common.Library.ASTTools exposing (filter, getHeadings, getTitle)

import Common.Syntax as Syntax


getTitle : Syntax.Language -> List Syntax.TextBlock -> Maybe String
getTitle language blocks =
    filterStrict "title" blocks |> List.head |> Maybe.map (Syntax.textToString >> String.trim)


getHeadings : Syntax.Language -> List Syntax.TextBlock -> List Syntax.Text
getHeadings language blocks =
    case language of
        Syntax.Markdown ->
            filter "#" blocks

        _ ->
            []


filter : String -> List Syntax.TextBlock -> List Syntax.Text
filter key blocks =
    List.map (filter_ key) blocks |> List.concat


filterStrict : String -> List Syntax.TextBlock -> List Syntax.Text
filterStrict key blocks =
    List.map (filterStrict_ key) blocks |> List.concat


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
