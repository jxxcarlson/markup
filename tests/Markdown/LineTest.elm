module Markdown.LineTest exposing (suite)

import Common.BlockParser as BlockParser
import Common.Line as Line
import Common.Syntax as Syntax
import Expect exposing (Expectation)
import Parser
import Test exposing (..)


classify =
    BlockParser.classify Syntax.Markdown


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
        , test "classify False empty line" <|
            \_ ->
                classify False ""
                    |> Expect.equal { indent = 0, lineType = Line.BlankLine, content = "" }
        , test "classify False  blank line with 3 leading spaces" <|
            \_ ->
                classify False "   "
                    |> Expect.equal { indent = 3, lineType = Line.BlankLine, content = "  " }

        -- TODO: only two spaces returned.  But do we care?
        , test "classify False ordinary line with 3 leading spaces" <|
            \_ ->
                classify False "   ho ho ho!"
                    |> Expect.equal { indent = 3, lineType = Line.OrdinaryLine, content = "  ho ho ho!" }

        -- TODO: only two spaces returned.  But do we care?
        , test "classify False begin code block" <|
            \_ ->
                classify False "```"
                    |> Expect.equal { indent = 0, lineType = Line.BeginVerbatimBlock "code", content = "" }
        , test "classify False begin math block" <|
            \_ ->
                classify False "$$"
                    |> Expect.equal { indent = 0, lineType = Line.BeginVerbatimBlock "math", content = "" }
        , test "classify False begin quotation block" <|
            \_ ->
                classify False ">"
                    |> Expect.equal { indent = 0, lineType = Line.BeginBlock "quotation", content = "" }
        ]
