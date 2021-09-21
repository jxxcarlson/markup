module Markup.API exposing
    ( compile, Settings
    , getTitle, parse, prepareForExport
    )

{-| The function Markup.API.compile will transform source text in any
one of three markup languages (L1, Markdown, MiniLaTeX) to `Html msg`.

@docs compile, Language, Settings

-}

import Common.Library.ASTTools
import Common.Render exposing (Settings)
import Common.Syntax as Syntax exposing (Language(..), Meta, Text(..))
import Common.Text.Cursor as Cursor
import Common.Text.Parser
import Element exposing (Element)
import L1.BlockParser as L1
import Markdown.BlockParser as Markdown
import Markdown.Rule
import MiniLaTeX.BlockParser as MiniLaTeX
import MiniLaTeX.Rule


{-| -}
getTitle : Syntax.Language -> List Syntax.TextBlock -> Maybe String
getTitle =
    Common.Library.ASTTools.getTitle


{-| -}
compile : Syntax.Language -> Int -> Settings -> List String -> List (Element msg)
compile language generation settings lines =
    lines |> parse language generation |> Common.Render.render generation settings


{-| -}
type alias Settings =
    { width : Int }


prepareForExport : String -> ( List String, String )
prepareForExport str =
    ( [ "image urls" ], "document content" )


parse : Syntax.Language -> Int -> List String -> List Syntax.TextBlock
parse language generation lines =
    case language of
        Syntax.Markdown ->
            lines |> Markdown.parse generation |> List.map (Syntax.map (parseLoop language))

        Syntax.MiniLaTeX ->
            lines |> MiniLaTeX.parse generation |> List.map (Syntax.map (parseLoop language))

        Syntax.L1 ->
            lines |> L1.parse generation |> List.map (Syntax.map (Common.Text.Parser.dummyParse generation { width = 500 }))



-- NOT EXPOSED


parseLoop : Syntax.Language -> String -> List Text
parseLoop language input =
    case language of
        Syntax.Markdown ->
            Cursor.parseLoop Markdown.Rule.markdownRules (Cursor.init 0 0 0 input) |> .committed |> List.reverse

        Syntax.MiniLaTeX ->
            Cursor.parseLoop MiniLaTeX.Rule.miniLaTeXRules (Cursor.init 0 0 0 input) |> .committed |> List.reverse

        Syntax.L1 ->
            Cursor.parseLoop Markdown.Rule.markdownRules (Cursor.init 0 0 0 input) |> .committed |> List.reverse
