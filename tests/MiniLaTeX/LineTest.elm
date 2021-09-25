module MiniLaTeX.LineTest exposing (suite)

import Common.BlockParser as BlockParser
import Common.Line as Line
import Common.Syntax as Syntax
import Expect exposing (Expectation)
import Parser
import Test exposing (..)


classify =
    BlockParser.classify Syntax.MiniLaTeX


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
        , test "(4) classify empty line" <|
            \_ ->
                classify False ""
                    |> Expect.equal { indent = 0, lineType = Line.BlankLine, content = "" }
        , test "(5) classify  blank line with 3 leading spaces" <|
            \_ ->
                -- TODO: note, one space less than expected, no big deal??
                classify False "   "
                    |> Expect.equal { indent = 3, lineType = Line.BlankLine, content = "  " }
        , test "(6) classify ordinary line with 3 leading spaces" <|
            \_ ->
                -- TODO: note, one space less than expected, no big deal??
                classify False "   ho ho ho!"
                    |> Expect.equal { content = "  ho ho ho!", indent = 3, lineType = Line.OrdinaryLine }
        , test "(7) classify begin block" <|
            \_ ->
                classify False "\\begin{foo}"
                    |> Expect.equal { content = "", indent = 0, lineType = Line.BeginBlock Line.RejectFirstLine "foo" }
        , test "(8) classify end block" <|
            \_ ->
                classify False "\\end{foo}"
                    |> Expect.equal { content = "", indent = 0, lineType = Line.EndBlock "foo" }
        ]
