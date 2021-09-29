module MiniLaTeX.BlockParserTest exposing (suite)

import Common.BasicSyntax as Syntax exposing (BasicBlock(..))
import Common.BlockParser as BlockParser
import Common.BlockParserTools
import Common.Syntax exposing (Language(..))
import Expect exposing (Expectation)
import Test exposing (..)


rs str =
    BlockParser.runParser MiniLaTeX 1 (String.lines str) |> .output |> List.map Syntax.simplify


testParser label input output =
    test label <|
        \_ ->
            rs input
                |> Expect.equal output


suite : Test
suite =
    describe
        "The MiniLaTeX Block Parser"
        [ testParser "(1)"
            "\\begin{foo}\n   ho ho ho!\n\\end{foo}"
            [ BBBlock "foo" [ BBParagraph [ "ho ho ho!\n" ] ] ]
        , testParser "(2)"
            "aaa\nbbb\n\n\\begin{foo}\n   ho ho ho!\n\\end{foo}n\nccc\nddd"
            [ BBParagraph [ "aaa\n", "bbb\n" ]
            , BBParagraph []
            , BBBlock "foo" [ BBParagraph [ "ho ho ho!\n" ] ]
            , BBParagraph [ "ccc\n", "ddd\n" ]
            ]
        , testParser "(3)"
            "\\begin{foo}\n   HA HA HA!\n\\end{BAR}"
            [ BBBlock "foo" [ BBParagraph [ "HA HA HA!\n" ] ]
            , BBError "Error: I was expecting an end-block labeled  foo, but found BAR"
            ]
        , testParser "(4)"
            "\\begin{foo}\n   ho ho ho!\n\\end{foo}\n\n\\begin{bar}\n   x^2\n\\end{bar}"
            [ BBBlock "foo" [ BBParagraph [ "ho ho ho!\n" ] ], BBBlock "bar" [ BBParagraph [ "x^2\n" ] ] ]
        , testParser "(5)"
            "\\begin{foo}\n   ho ho ho!\n\n\n"
            [ BBBlock "foo" [ BBParagraph [ "ho ho ho!\n" ] ] ]
        , testParser "(6)"
            "\\begin{foo}\n   ho ho ho!\n\n\n\\begin{bar}\n   HA HA HA!\n\\end{bar}"
            [ BBBlock "foo" [ BBParagraph [ "ho ho ho!\n" ] ], BBBlock "bar" [ BBParagraph [ "HA HA HA!\n" ] ] ]
        , -- TODO: returning leading spaces, hmm???
          testParser "(7)"
            "$$\n    x^2"
            [ BBVerbatimBlock "math" [ "    x^2" ] ]
        , -- TODO: returning leading spaces, hmm???
          testParser "(8)"
            "$$\n    x^2\n\nHo ho ho!"
            [ BBVerbatimBlock "math" [ "    x^2" ], BBParagraph [ "", "Ho ho ho!\n" ] ]
        , testParser "(9)"
            "$$\n    x^2\n$$"
            [ BBVerbatimBlock "math" [ "    x^2" ] ]
        , testParser "(10)"
            "```\n   a[i] = a[i] + 1\n   \n   b[i] = b[i] + 1"
            [ BBVerbatimBlock "code" [ "   a[i] = a[i] + 1", "   ", "   b[i] = b[i] + 1" ] ]
        , testParser "(11)"
            "one\ntwo"
            [ BBParagraph [ "one\n", "two\n" ] ]
        , testParser "(12)"
            "\\begin{equation}\n   \\int_0^1 x^n dx = \\frac{1}{n+1}\n\\end{equation}"
            [ BBVerbatimBlock "equation" [ "   \\int_0^1 x^n dx = \\frac{1}{n+1}" ] ]
        ]
