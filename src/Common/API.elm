module Common.API exposing (renderL1, renderMarkdown, renderMiniLaTeX)

import Common.Render exposing (Settings)
import Common.Syntax as Syntax
import Common.TextParser
import Element exposing (Element)
import L1.BlockParser as L1
import Markdown.BlockParser as Markdown
import MiniLaTeX.BlockParser as MiniLaTeX


parseMarkdown : Int -> Settings -> List String -> List Syntax.TextBlock
parseMarkdown generation settings lines =
    lines
        |> Markdown.parse generation
        |> List.map (Syntax.mapList (Common.TextParser.parse generation settings))


renderMarkdown : Int -> Settings -> List String -> List (Element msg)
renderMarkdown generation settings lines =
    lines
        |> parseMarkdown generation settings
        |> Common.Render.render generation settings


parseMiniLaTeX : Int -> Settings -> List String -> List Syntax.TextBlock
parseMiniLaTeX generation settings lines =
    lines
        |> MiniLaTeX.parse generation
        |> List.map (Syntax.mapList (Common.TextParser.parse generation settings))


renderMiniLaTeX : Int -> Settings -> List String -> List (Element msg)
renderMiniLaTeX generation settings lines =
    lines
        |> parseMiniLaTeX generation settings
        |> Common.Render.render generation settings


parseL1 : Int -> Settings -> List String -> List Syntax.TextBlock
parseL1 generation settings lines =
    lines
        |> L1.parse generation
        |> List.map (Syntax.mapList (Common.TextParser.parse generation settings))


renderL1 : Int -> Settings -> List String -> List (Element msg)
renderL1 generation settings lines =
    lines
        |> parseL1 generation settings
        |> Common.Render.render generation settings
