module L1.Transform exposing (expandTextInMarked, map)

import Common.Syntax exposing (Text(..), TextBlock(..), map)


map : (Text -> Text) -> TextBlock -> TextBlock
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


expandText : Text -> List Text
expandText text =
    case text of
        Text str meta ->
            String.words str |> List.map (\s -> Text (s ++ " ") meta)

        _ ->
            []


expandTextInMarked : Text -> Text
expandTextInMarked text =
    case text of
        Marked name textList meta ->
            Marked name (List.map expandText textList |> List.concat) meta

        _ ->
            text
