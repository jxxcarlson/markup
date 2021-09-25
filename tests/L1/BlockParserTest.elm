module L1.BlockParserTest exposing (..)

import Common.BasicSyntax as Syntax exposing (BasicBlock(..))
import Common.BlockParser as BlockParser
import Common.Syntax exposing (Language(..))
import Expect exposing (Expectation)
import Test exposing (..)


rs str =
    BlockParser.runParser L1 1 (String.lines str) |> .output |> List.map Syntax.simplify


testParser : String -> List BasicBlock -> Test
testParser input output =
    test input <|
        \_ ->
            rs input
                |> Expect.equal output


suite : Test
suite =
    describe "The Line modules for L1"
        [ testParser
            "| indent\n   foo"
            [ BBBlock "indent" [ BBParagraph [ "foo\n" ] ] ]
        , testParser
            "| indent\n   foo\n   bar"
            [ BBBlock "indent" [ BBParagraph [ "foo\n", "bar\n" ] ] ]
        , testParser
            "| indent\n   foo\n   bar\n   baz"
            [ BBBlock "indent" [ BBParagraph [ "foo\n", "bar\n", "baz\n" ] ] ]
        , testParser
            "| indent\n   foo\n   bar\n|| math\n   x^2"
            [ BBBlock "indent" [ BBParagraph [ "foo\n", "bar\n" ] ]
            , BBVerbatimBlock "math" [ "   x^2" ]
            ]
        , testParser
            "| indent\n   foo\n   bar\n|\n   \n   baz"
            [ BBBlock "indent" [ BBParagraph [ "foo\n", "bar\n" ] ]
            , BBParagraph []
            , BBParagraph [ "baz\n" ]
            ]
        , testParser
            "| indent\n   foo\n   bar\n|\n\n   baz"
            [ BBBlock "indent" [ BBParagraph [ "foo\n", "bar\n" ] ]
            , BBParagraph [ "baz\n" ]
            ]
        , testParser
            "|| code\n   foo\n   bar\n\n   baz"
            [ BBVerbatimBlock "code" [ "   foo", "   bar" ], BBParagraph [ "", "baz\n" ] ]
        , testParser
            "|| code\n   foo\n   bar\n   \n   baz"
            [ BBVerbatimBlock "code" [ "   foo", "   bar", "   ", "   baz" ] ]
        , testParser
            "|| code\n   foo\n   bar\n   \n   baz\nho ho ho!"
            [ BBVerbatimBlock "code" [ "   foo", "   bar", "   ", "   baz" ]
            , BBParagraph [ "ho ho ho!\n" ]
            ]
        , testParser
            "ABC\nXYZ\n|| code\n   foo\n   bar\nDEF\n\nGHI"
            [ BBParagraph [ "ABC\n", "XYZ\n" ]
            , BBVerbatimBlock "code" [ "   foo", "   bar" ]
            , BBParagraph [ "DEF\n" ]
            , BBParagraph []
            , BBParagraph [ "GHI\n" ]
            ]
        ]
