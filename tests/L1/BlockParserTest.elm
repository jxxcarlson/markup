module L1.BlockParserTest exposing (..)

import Expect exposing (Expectation)
import L1.BlockParser as BlockParser exposing (Block(..))
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
    describe "The Line modules for L1"
        [ testParser
            "| indent\n   foo"
            [ Block "indent" [ Paragraph [ "foo" ] ] ]
        , testParser
            "| indent\nfoo (*)"
            [ Block "indent" [], Paragraph [ "foo (*)" ] ]
        , testParser
            "| indent\n   foo\n   bar"
            [ Block "indent" [ Paragraph [ "foo", "bar" ] ] ]
        , testParser
            "| indent\n   foo\n   bar\n   baz"
            [ Block "indent" [ Paragraph [ "foo", "bar", "baz" ] ] ]
        , testParser
            "| indent\n   foo\n   bar\n| math\n   x^2"
            [ Block "indent" [ Paragraph [ "foo", "bar" ] ], Block "math" [ Paragraph [ "x^2" ] ] ]
        , testParser
            "| indent\n   foo\n   bar\n|\n   \n   baz"
            [ Block "indent" [ Paragraph [ "foo", "bar", "", "baz" ] ] ]
        , testParser
            "| indent\n   foo\n   bar\n|\n\n   baz"
            [ Block "indent" [ Paragraph [ "foo", "bar", "baz" ] ] ]
        , testParser
            "|| code\n   foo\n   bar\n\n   baz"
            [ VerbatimBlock "code" [ "foo", "bar", "baz" ] ]
        , testParser
            "|| code\n   foo\n   bar\n   \n   baz"
            [ VerbatimBlock "code" [ "foo", "bar", "", "baz" ]
            ]
        , testParser
            "|| code\n   foo\n   bar\n   \n   baz\nho ho ho!"
            [ VerbatimBlock "code" [ "foo", "bar", "", "baz" ]
            , Paragraph [ "ho ho ho!" ]
            ]
        , testParser
            "ABC\nXYZ\n|| code\n   foo\n   bar\nDEF\n\nGHI"
            [ Paragraph [ "ABC", "XYZ" ]
            , VerbatimBlock "code" [ "foo", "bar" ]
            , Paragraph [ "DEF", "", "GHI" ]
            ]
        ]
