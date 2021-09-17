module Common.API exposing (renderL1, renderMarkdown, renderMiniLaTeX)

import Common.Render exposing (Settings)
import Element exposing (Element)
import L1.BlockParser as L1
import Markdown.BlockParser as Markdown
import MiniLaTeX.BlockParser as MiniLaTeX


renderMarkdown : Int -> Settings -> List String -> List (Element msg)
renderMarkdown generation settings lines =
    lines
        |> Markdown.parse generation
        |> Common.Render.render generation settings


renderMiniLaTeX : Int -> Settings -> List String -> List (Element msg)
renderMiniLaTeX generation settings lines =
    lines
        |> MiniLaTeX.parse generation
        |> Common.Render.render generation settings


renderL1 : Int -> Settings -> List String -> List (Element msg)
renderL1 generation settings lines =
    lines
        |> L1.parse generation
        |> Common.Render.render generation settings
