module Markdown.BlockParserTest exposing (..)

import Common.BlockParser
import Common.Syntax exposing (Block(..))
import Expect exposing (Expectation)
import Markdown.BlockParser as BlockParser
import Test exposing (..)


rs str =
    BlockParser.runFromString 1 str |> .output |> List.map .content


testParser input output =
    test input <|
        \_ ->
            rs input
                |> Expect.equal output


suite : Test
suite =
    describe "The Markdown Block Parser"
        [ testParser
            ">\n   This is a test\n   and so is this"
            [ Block "quotation" [ Paragraph [ "This is a test", "and so is this" ] ] ]
        , testParser
            "```\n   a[i] = 1\n   \n   b[i] = 2"
            [ VerbatimBlock "code" [ "a[i] = 1", "", "b[i] = 2" ] ]
        , testParser
            "$$\n   x^2 = 3\n   y^3 = 5"
            [ VerbatimBlock "math" [ "x^2 = 3", "y^3 = 5" ] ]
        ]
