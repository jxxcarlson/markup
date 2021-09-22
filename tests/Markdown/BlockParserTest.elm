module Markdown.BlockParserTest exposing (..)

import Common.BasicSyntax as Syntax exposing (BasicBlock(..))
import Common.BlockParser
import Expect exposing (Expectation)
import Markdown.BlockParser as BlockParser
import Test exposing (..)


rs str =
    BlockParser.run 1 (String.lines str) |> .output |> List.map Syntax.simplify


testParser input output =
    test input <|
        \_ ->
            rs input
                |> Expect.equal output


suite : Test
suite =
    describe "The Markdown Block Parser"
        [ testParser
            "This is a test\n   and so is this"
            [ BBParagraph [ "This is a test", "and so is this" ] ]
        , testParser
            "> Hahaha!\n   This is a test\n   and so is this"
            [ BBBlock "quotation" [ BBParagraph [ "Hahaha!", "This is a test", "and so is this" ] ] ]
        , testParser
            "```\n   a[i] = 1\n   \n   b[i] = 2"
            [ BBParagraph [ "   a[i] = 1" ], BBVerbatimBlock "code" [], BBParagraph [], BBParagraph [ "   b[i] = 2" ] ]
        , testParser
            "$$\n   x^2 = 3\n   y^3 = 5"
            [ BBVerbatimBlock "math" [ "   x^2 = 3", "   y^3 = 5" ] ]
        ]
