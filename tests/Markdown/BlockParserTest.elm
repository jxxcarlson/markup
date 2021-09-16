module Markdown.BlockParserTest exposing (..)

import Common.BlockParser
import Common.Syntax exposing (BasicBlock(..))
import Expect exposing (Expectation)
import Markdown.BlockParser as BlockParser
import Test exposing (..)


rs str =
    BlockParser.run 1 (String.lines str) |> .output |> List.map .content


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
            ">\n   This is a test\n   and so is this"
            [ BBBlock "quotation" [ BBParagraph [ "This is a test", "and so is this" ] ] ]
        , testParser
            "```\n   a[i] = 1\n   \n   b[i] = 2"
            [ BBVerbatimBlock "code" [ "a[i] = 1", "", "b[i] = 2" ] ]
        , testParser
            "$$\n   x^2 = 3\n   y^3 = 5"
            [ BBVerbatimBlock "math" [ "x^2 = 3", "y^3 = 5" ] ]
        ]