module L1.BlockParserTest exposing (..)

import Common.BasicSyntax as Syntax exposing (BasicBlock(..))
import Expect exposing (Expectation)
import L1.BlockParser as BlockParser
import Test exposing (..)


rs str =
    BlockParser.run 1 (String.lines str) |> .output |> List.map Syntax.simplify


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
            [ BBBlock "indent" [ BBParagraph [ "foo" ] ] ]
        , testParser
            "| indent\nfoo (*)"
            [ BBBlock "indent" [], BBParagraph [ "foo (*)" ] ]
        , testParser
            "| indent\n   foo\n   bar"
            [ BBBlock "indent" [ BBParagraph [ "foo", "bar" ] ] ]
        , testParser
            "| indent\n   foo\n   bar\n   baz"
            [ BBBlock "indent" [ BBParagraph [ "foo", "bar", "baz" ] ] ]
        , testParser
            "| indent\n   foo\n   bar\n| math\n   x^2"
            [ BBBlock "indent" [ BBParagraph [ "foo", "bar" ] ], BBBlock "math" [ BBParagraph [ "x^2" ] ] ]
        , testParser
            "| indent\n   foo\n   bar\n|\n   \n   baz"
            [ BBBlock "indent" [ BBParagraph [ "foo", "bar", "", "baz" ] ] ]
        , testParser
            "| indent\n   foo\n   bar\n|\n\n   baz"
            [ BBBlock "indent" [ BBParagraph [ "foo", "bar", "baz" ] ] ]
        , testParser
            "|| code\n   foo\n   bar\n\n   baz"
            [ BBVerbatimBlock "code" [ "foo", "bar", "baz" ] ]
        , testParser
            "|| code\n   foo\n   bar\n   \n   baz"
            [ BBVerbatimBlock "code" [ "foo", "bar", "", "baz" ]
            ]
        , testParser
            "|| code\n   foo\n   bar\n   \n   baz\nho ho ho!"
            [ BBVerbatimBlock "code" [ "foo", "bar", "", "baz" ]
            , BBParagraph [ "ho ho ho!" ]
            ]
        , testParser
            "ABC\nXYZ\n|| code\n   foo\n   bar\nDEF\n\nGHI"
            [ BBParagraph [ "ABC", "XYZ" ]
            , BBVerbatimBlock "code" [ "foo", "bar" ]
            , BBParagraph [ "DEF", "", "GHI" ]
            ]
        ]
