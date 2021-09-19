module MiniLaTeX.BlockParserTest exposing (..)

import Common.BasicSyntax as Syntax exposing (BasicBlock(..))
import Common.BlockParser
import Expect exposing (Expectation)
import MiniLaTeX.BlockParser as BlockParser
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
    describe "The MiniLaTeX Block Parser"
        [ testParser
            "\\begin{foo}\n   ho ho ho!\n\\end{foo}"
            [ BBBlock "foo" [ BBParagraph [ "ho ho ho!" ] ] ]
        , testParser
            "aaa\nbbb\n\n\\begin{foo}\n   ho ho ho!\n\\end{foo}n\nccc\nddd"
            [ BBParagraph [ "aaa", "bbb", "" ]
            , BBBlock "foo" [ BBParagraph [ "ho ho ho!" ] ]
            , BBParagraph [ "ccc", "ddd" ]
            ]
        , Test.skip <|
            testParser
                "\\begin{foo}\n   HA HA HA!\n\\end{BAR}"
                [ BBBlock "foo" [ BBParagraph [ "HA HA HA!" ] ]
                , BBParagraph [ "Error: I was expecting an end-block labeled  foo, but found BAR" ]
                ]
        , testParser
            "\\begin{foo}\n   ho ho ho!\n\\end{foo}\n\n\\begin{bar}\n   x^2\n\\end{bar}"
            [ BBBlock "foo" [ BBParagraph [ "ho ho ho!" ] ], BBBlock "bar" [ BBParagraph [ "x^2" ] ] ]
        , testParser
            "\\begin{foo}\n   ho ho ho!\n\n\n"
            [ BBBlock "foo" [ BBParagraph [ "ho ho ho!" ] ] ]
        , testParser
            "\\begin{foo}\n   ho ho ho!\n\n\n\\begin{bar}\n   HA HA HA!\n\\end{bar}"
            [ BBBlock "foo" [ BBParagraph [ "ho ho ho!" ] ], BBBlock "bar" [ BBParagraph [ "HA HA HA!" ] ] ]
        , testParser
            "$$\n    x^2"
            [ BBVerbatimBlock "math" [ "x^2" ] ]
        , testParser
            "$$\n    x^2\n\nHo ho ho!"
            [ BBVerbatimBlock "math" [ "x^2" ], BBParagraph [ "Ho ho ho!" ] ]
        , testParser
            "$$\n    x^2\n$$"
            [ BBVerbatimBlock "math" [ "x^2" ] ]
        , testParser
            "Code:\n```\n   a[i] = a[i] + 1\n   \n   b[i] = b[i] + 1\n\nOk!"
            [ BBParagraph [ "Code:" ], BBVerbatimBlock "code" [ "a[i] = a[i] + 1", "", "b[i] = b[i] + 1" ], BBParagraph [ "Ok!" ] ]
        , Test.skip <|
            testParser
                "one\ntwo\n\nthree\nfour"
                [ BBParagraph [ "one", "two" ], BBParagraph [ "three", "four" ] ]
        ]
