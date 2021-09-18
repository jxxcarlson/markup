module Markup.API exposing (compile, Language(..), Settings)

{-| The function Markup.API.compile will transform source text in any
one of three markup languages (L1, Markdown, MiniLaTeX) to `Html msg`.

@docs compile, Language, Settings

-}

import Common.Render exposing (Settings)
import Common.Syntax as Syntax
import Common.TextParser
import Element exposing (Element)
import L1.BlockParser as L1Block
import Markdown.BlockParser as Markdown
import MiniLaTeX.BlockParser as MiniLaTeX


{-| -}
compile : Language -> Int -> Settings -> List String -> List (Element msg)
compile language generation settings lines =
    case language of
        L1 ->
            compileL1 generation settings lines

        Markdown ->
            compileMarkdown generation settings lines

        MiniLaTeX ->
            compileMiniLaTeX generation settings lines


{-| -}
type Language
    = L1
    | Markdown
    | MiniLaTeX


{-| -}
type alias Settings =
    { width : Int }


compileMarkdown : Int -> Settings -> List String -> List (Element msg)
compileMarkdown generation settings lines =
    lines
        |> Markdown.parse generation
        |> List.map (Syntax.mapList (Common.TextParser.parse generation settings))
        |> Common.Render.render generation settings


compileMiniLaTeX : Int -> Settings -> List String -> List (Element msg)
compileMiniLaTeX generation settings lines =
    lines
        |> MiniLaTeX.parse generation
        |> List.map (Syntax.mapList (Common.TextParser.parse generation settings))
        |> Common.Render.render generation settings


compileL1 : Int -> Settings -> List String -> List (Element msg)
compileL1 generation settings lines =
    lines
        |> L1Block.parse generation
        |> List.map (Syntax.mapList (Common.TextParser.parse generation settings))
        |> Common.Render.render generation settings
