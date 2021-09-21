module Common.Library.ASTTools exposing (..)

import Common.Syntax as Syntax



filter : String -> Syntax.TextBlock -> List Syntax.Text
filter key block =
    case block of
        Syntax.TBParagraph -> List.filter (\t -> )

