module L1.Transform exposing (expandTextInMarked, map)

import Common.Syntax exposing (Expr(..), TextBlock(..), map)


map : (Expr -> Expr) -> TextBlock -> TextBlock
map f block =
    case block of
        TBParagraph textList meta ->
            TBParagraph (List.map f textList) meta

        TBVerbatimBlock _ _ _ ->
            block

        TBBlock name blockList meta ->
            TBBlock name (List.map (map f) blockList) meta

        TBError _ ->
            block


expandText : Expr -> List Expr
expandText text =
    case text of
        Text str meta ->
            String.words str |> List.map (\s -> Text (s ++ " ") meta)

        _ ->
            []


expandTextInMarked : Expr -> Expr
expandTextInMarked text =
    case text of
        Expr name textList meta ->
            Expr name (List.map expandText textList |> List.concat) meta

        _ ->
            text
