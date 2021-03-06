module Markup.API exposing
    ( compile, Settings
    , blockParse, getTitle, parse, prepareForExport, render, renderFancy, tableOfContents
    )

{-| The function Markup.API.compile will transform source text in any
one of three markup languages (L1, Markdown, MiniLaTeX) to `Html msg`.

@docs compile, Language, Settings

-}

import Common.BlockParser as Block
import Common.Library.ASTTools as ASTTools
import Common.Render.Text as Text
import Common.Render.TextBlock as TextBlock exposing (Settings)
import Common.Syntax as Syntax exposing (Expr(..), Language(..), Meta)
import Common.Text.Cursor as Cursor
import Common.Text.Parser
import Element as E exposing (Element)
import Element.Font
import L1.Rule
import L1.Transform
import Markdown.Rule
import MiniLaTeX.MathMacro
import MiniLaTeX.Rule


type alias Accumulator =
    { macroDict : MiniLaTeX.MathMacro.MathMacroDict }


{-| -}
getTitle : List Syntax.TextBlock -> Maybe String
getTitle =
    ASTTools.getTitle


renderFancy : Syntax.Language -> Int -> List String -> List (Element msg)
renderFancy language count source =
    let
        parseData =
            parseAccum language count source

        ast =
            parseData.blocks

        toc_ : List (Element msg)
        toc_ =
            tableOfContents count { width = 500 } parseData.accumulator ast

        titleString =
            ASTTools.getTitle ast |> Maybe.withDefault "Untitled" |> String.replace "\n" " "

        docTitle =
            E.el [ Element.Font.size 30 ] (E.text titleString)

        toc =
            E.column [ E.paddingXY 0 24, E.spacing 8 ] toc_

        renderedText_ : List (Element msg)
        renderedText_ =
            render count { width = 500 } parseData.accumulator ast
    in
    docTitle :: toc :: renderedText_


tableOfContents : Int -> Settings -> Accumulator -> List Syntax.TextBlock -> List (Element msg)
tableOfContents generation settings accumulator blocks =
    blocks |> ASTTools.getHeadings |> Text.viewTOC generation settings accumulator


{-| -}
compile : Syntax.Language -> Int -> Settings -> List String -> List (Element msg)
compile language generation settings lines =
    let
        parseData =
            parseAccum language generation lines
    in
    parseData.blocks |> TextBlock.render generation settings parseData.accumulator


render =
    TextBlock.render


{-| -}
type alias Settings =
    { width : Int }


prepareForExport : String -> ( List String, String )
prepareForExport str =
    ( [ "image urls" ], "document content" )


parse : Syntax.Language -> Int -> List String -> List Syntax.TextBlock
parse language generation lines =
    lines
        |> Block.parse language generation
        |> List.map (Syntax.map (parseText language))


parseAccum : Syntax.Language -> Int -> List String -> { blocks : List Syntax.TextBlock, accumulator : Accumulator }
parseAccum language generation lines =
    let
        state =
            Block.runParser language generation lines
    in
    { blocks = List.map (Syntax.map (parseText language)) state.output, accumulator = state.accumulator }



-- |> astTransform language


astTransform : Language -> List Syntax.TextBlock -> List Syntax.TextBlock
astTransform language =
    case language of
        L1 ->
            List.map (L1.Transform.map L1.Transform.expandTextInMarked)

        _ ->
            identity


blockParse : Syntax.Language -> Int -> List String -> List Syntax.Block
blockParse language generation lines =
    lines |> Block.parse language generation


parse2 : Syntax.Language -> List Syntax.Block -> List Syntax.TextBlock
parse2 language blocks =
    List.map (Syntax.map (parseText language)) blocks



-- NOT EXPOSED


parseText : Syntax.Language -> String -> List Expr
parseText language input =
    case language of
        Syntax.Markdown ->
            Cursor.parseLoop Markdown.Rule.rules (Cursor.init 0 0 0 input) |> .committed |> List.reverse

        Syntax.MiniLaTeX ->
            Cursor.parseLoop MiniLaTeX.Rule.rules (Cursor.init 0 0 0 input) |> .committed |> List.reverse

        Syntax.L1 ->
            Cursor.parseLoop L1.Rule.rules (Cursor.init 0 0 0 input) |> .committed |> List.reverse
