module Markup.API exposing
    ( compile, Language(..), Settings
    , parseMiniLaTeX
    )

{-| The function Markup.API.compile will transform source text in any
one of three markup languages (L1, Markdown, MiniLaTeX) to `Html msg`.

@docs compile, Language, Settings

-}

import Common.Render exposing (Settings)
import Common.Syntax as Syntax exposing (Meta, Text(..))
import Common.Text.Cursor as Cursor
import Common.Text.Parser
import Element exposing (Element)
import L1.BlockParser as L1Block
import Markdown.BlockParser as Markdown
import MiniLaTeX.BlockParser as MiniLaTeX
import MiniLaTeX.Rule


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
        |> List.map (Syntax.map2 (Common.Text.Parser.dummyParse generation settings))
        |> Common.Render.render generation settings


compileMiniLaTeX : Int -> Settings -> List String -> List (Element msg)
compileMiniLaTeX generation settings lines =
    lines
        |> MiniLaTeX.parse generation
        |> List.map (Syntax.map2 miniLaTeXParseLoop)
        |> Common.Render.render generation settings


parseMiniLaTeX : Int -> b -> List String -> List Syntax.TextBlock
parseMiniLaTeX generation settings lines =
    lines
        |> MiniLaTeX.parse generation
        |> List.map (Syntax.map2 miniLaTeXParseLoop)


miniLaTeXParseLoop : String -> List Text
miniLaTeXParseLoop input =
    Cursor.parseLoop MiniLaTeX.Rule.miniLaTeXRules (Cursor.init 0 0 0 input) |> .committed


compileL1 : Int -> Settings -> List String -> List (Element msg)
compileL1 generation settings lines =
    lines
        |> L1Block.parse generation
        |> List.map (Syntax.map2 (Common.Text.Parser.dummyParse generation settings))
        |> Common.Render.render generation settings
