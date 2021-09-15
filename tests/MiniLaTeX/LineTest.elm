module MiniLaTeX.LineTest exposing (suite)

import Expect exposing (Expectation)
import Line
import MiniLaTeX.Line as MiniLaTeX
import Parser
import Test exposing (..)


intTest label theTest expectedValue =
    test label <|
        \_ ->
            theTest
                |> Expect.equal expectedValue


suite : Test
suite =
    describe "The Line modules for MiniLaTex"
        [ intTest "countLeadingSpaces" (Line.countLeadingSpaces "   abc") 3
        , test "blankLineParser" <|
            \_ ->
                Parser.run Line.emptyLineParser ""
                    |> Expect.equal (Ok Line.BlankLine)
        , test "ordinaryLineParser, succeed" <|
            \_ ->
                Parser.run (Line.ordinaryLineParser []) "ho ho ho!"
                    |> Expect.equal (Ok Line.OrdinaryLine)
        , test "ordinaryLineParser, macro" <|
            \_ ->
                Parser.run (Line.ordinaryLineParser []) "\\{foo} ho ho ho!"
                    |> Expect.equal (Ok Line.OrdinaryLine)
        , test "MiniLaTeX.classify empty line" <|
            \_ ->
                MiniLaTeX.classify ""
                    |> Expect.equal { indent = 0, lineType = Line.BlankLine }
        , test "MiniLaTeX.classify  blank line with 3 leading spaces" <|
            \_ ->
                MiniLaTeX.classify "   "
                    |> Expect.equal { indent = 3, lineType = Line.BlankLine }
        , test "MiniLaTeX.classify ordinary line with 3 leading spaces" <|
            \_ ->
                MiniLaTeX.classify "   ho ho ho!"
                    |> Expect.equal { indent = 3, lineType = Line.OrdinaryLine }
        , test "MiniLaTeX.classify begin block" <|
            \_ ->
                MiniLaTeX.classify "\\begin{foo}"
                    |> Expect.equal { indent = 0, lineType = Line.BeginBlock "foo" }
        , test "MiniLaTeX.classify end block" <|
            \_ ->
                MiniLaTeX.classify "\\end{foo}"
                    |> Expect.equal { indent = 0, lineType = Line.EndBlock "foo" }
        ]
