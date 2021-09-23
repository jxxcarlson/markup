module MiniLaTeX.LineTest exposing (suite)

import Common.Line as Line
import Expect exposing (Expectation)
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
        , test "(1) blankLineParser" <|
            \_ ->
                Parser.run Line.emptyLineParser ""
                    |> Expect.equal (Ok Line.BlankLine)
        , test "(2) ordinaryLineParser, succeed" <|
            \_ ->
                Parser.run (Line.ordinaryLineParser []) "ho ho ho!"
                    |> Expect.equal (Ok Line.OrdinaryLine)
        , test "(3) ordinaryLineParser, macro" <|
            \_ ->
                Parser.run (Line.ordinaryLineParser []) "\\{foo} ho ho ho!"
                    |> Expect.equal (Ok Line.OrdinaryLine)
        , test "(4) MiniLaTeX.classify empty line" <|
            \_ ->
                MiniLaTeX.classify ""
                    |> Expect.equal { indent = 0, lineType = Line.BlankLine, content = "" }
        , test "(5) MiniLaTeX.classify  blank line with 3 leading spaces" <|
            \_ ->
                -- TODO: note, one space less than expected, no big deal??
                MiniLaTeX.classify "   "
                    |> Expect.equal { indent = 3, lineType = Line.BlankLine, content = "  " }
        , test "(6) MiniLaTeX.classify ordinary line with 3 leading spaces" <|
            \_ ->
                -- TODO: note, one space less than expected, no big deal??
                MiniLaTeX.classify "   ho ho ho!"
                    |> Expect.equal { content = "  ho ho ho!", indent = 3, lineType = Line.OrdinaryLine }
        , test "(7) MiniLaTeX.classify begin block" <|
            \_ ->
                MiniLaTeX.classify "\\begin{foo}"
                    |> Expect.equal { content = "", indent = 0, lineType = Line.BeginBlock "foo" }
        , test "(8) MiniLaTeX.classify end block" <|
            \_ ->
                MiniLaTeX.classify "\\end{foo}"
                    |> Expect.equal { content = "", indent = 0, lineType = Line.EndBlock "foo" }
        ]
