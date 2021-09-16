module Common.API exposing (renderMarkdown)

import Common.Render exposing (Settings)
import Element exposing (Element)
import Markdown.BlockParser as Markdown


renderMarkdown : Int -> Settings -> List String -> List (Element msg)
renderMarkdown generation settings lines =
    lines
        |> Markdown.parse generation
        |> Common.Render.render settings
