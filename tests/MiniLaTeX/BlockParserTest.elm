module MiniLaTeX.BlockParserTest exposing (..)

import Common.BlockParser exposing (Block(..))
import Expect exposing (Expectation)
import MiniLaTeX.BlockParser as BlockParser
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
    describe "The MiniLaTeX Block Parser"
        [ testParser
            "\\begin{foo}\n   ho ho ho!\n\\end{foo}"
            [ Block "foo" [ Paragraph [ "ho ho ho!" ] ] ]
        , testParser
            "aaa\nbbb\n\n\\begin{foo}\n   ho ho ho!\n\\end{foo}n\nccc\nddd"
            [ Paragraph [ "aaa", "bbb", "" ]
            , Block "foo" [ Paragraph [ "ho ho ho!" ] ]
            , Paragraph [ "ccc", "ddd" ]
            ]
        , testParser
            "\\begin{foo}\n   HA HA HA!\n\\end{BAR}"
            [ Block "foo" [ Paragraph [ "HA HA HA!" ] ]
            , Paragraph [ "Error: I was expecting an end-block labeled  foo, but found BAR" ]
            ]
        , testParser
            "\\begin{foo}\n   ho ho ho!\n\\end{foo}\n\n\\begin{bar}\n   x^2\n\\end{bar}"
            [ Block "foo" [ Paragraph [ "ho ho ho!" ] ], Block "bar" [ Paragraph [ "x^2" ] ] ]
        , testParser
            "\\begin{foo}\n   ho ho ho!\n\n\n"
            [ Block "foo" [ Paragraph [ "ho ho ho!" ] ] ]
        , testParser
            "\\begin{foo}\n   ho ho ho!\n\n\n\\begin{bar}\n   HA HA HA!\n\\end{bar}"
            [ Block "foo" [ Paragraph [ "ho ho ho!" ] ], Block "bar" [ Paragraph [ "HA HA HA!" ] ] ]
        , testParser
            "$$\n    x^2"
            [ VerbatimBlock "math" [ "x^2" ] ]
        , testParser
            "$$\n    x^2\n\nHo ho ho!"
            [ VerbatimBlock "math" [ "x^2" ], Paragraph [ "Ho ho ho!" ] ]
        , testParser
            "$$\n    x^2\n$$"
            [ VerbatimBlock "math" [ "x^2" ] ]
        , testParser
            "Code:\n```\n   a[i] = a[i] + 1\n   \n   b[i] = b[i] + 1\n\nOk!"
            [ Paragraph [ "Code:" ], VerbatimBlock "code" [ "a[i] = a[i] + 1", "", "b[i] = b[i] + 1" ], Paragraph [ "Ok!" ] ]
        ]
