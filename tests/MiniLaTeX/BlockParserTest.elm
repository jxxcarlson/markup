module MiniLaTeX.BlockParserTest exposing (suite)

import Common.BasicSyntax as Syntax exposing (BasicBlock(..))
import Common.BlockParser as BlockParser
import Common.BlockParserTools
import Common.Syntax exposing (Language(..))
import Expect exposing (Expectation)
import Test exposing (..)


rs str =
    BlockParser.run MiniLaTeX 1 (String.lines str) |> .output |> List.map Syntax.simplify


testParser input output =
    test input <|
        \_ ->
            rs input
                |> Expect.equal output


suite : Test
suite =
    describe
        "The MiniLaTeX Block Parser"
        [ testParser
            "\\begin{foo}\n   ho ho ho!\n\\end{foo}"
            [ BBBlock "foo" [ BBParagraph [ "ho ho ho!" ] ] ]
        , testParser
            "aaa\nbbb\n\n\\begin{foo}\n   ho ho ho!\n\\end{foo}n\nccc\nddd"
            [ BBParagraph [ "bbb", "aaa" ]
            , BBParagraph []
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
        , -- TODO: returning leading spaces, hmm???
          testParser
            "$$\n    x^2"
            [ BBVerbatimBlock "math" [ "    x^2" ] ]
        , -- TODO: returning leading spaces, hmm???
          testParser
            "$$\n    x^2\n\nHo ho ho!"
            [ BBVerbatimBlock "math" [ "    x^2" ], BBParagraph [ "", "Ho ho ho!" ] ]
        , testParser
            "$$\n    x^2\n$$"
            [ BBVerbatimBlock "math" [ "    x^2" ] ]
        , testParser
            "```\n   a[i] = a[i] + 1\n   \n   b[i] = b[i] + 1"
            [ BBVerbatimBlock "code" [ "   a[i] = a[i] + 1", "   ", "   b[i] = b[i] + 1" ] ]
        , Test.only <|
            testParser
                "one\ntwo"
                [ BBParagraph [ "one", "two" ] ]
        ]
