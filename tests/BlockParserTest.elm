module BlockParserTest exposing (..)

import BlockParser exposing (Block(..))
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import L1.Line as L1
import Line
import Parser
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
        ]
