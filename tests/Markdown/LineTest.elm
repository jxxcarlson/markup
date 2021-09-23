module Markdown.LineTest exposing (suite)

import Common.Line as Line
import Expect exposing (Expectation)
import Markdown.Line as Markdown
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
        , test "Markdown.classify False empty line" <|
            \_ ->
                Markdown.classify False ""
                    |> Expect.equal { indent = 0, lineType = Line.BlankLine, content = "" }
        , test "Markdown.classify False  blank line with 3 leading spaces" <|
            \_ ->
                Markdown.classify False "   "
                    |> Expect.equal { indent = 3, lineType = Line.BlankLine, content = "  " }

        -- TODO: only two spaces returned.  But do we care?
        , test "Markdown.classify False ordinary line with 3 leading spaces" <|
            \_ ->
                Markdown.classify False "   ho ho ho!"
                    |> Expect.equal { indent = 3, lineType = Line.OrdinaryLine, content = "  ho ho ho!" }

        -- TODO: only two spaces returned.  But do we care?
        , test "Markdown.classify False begin code block" <|
            \_ ->
                Markdown.classify False "```"
                    |> Expect.equal { indent = 0, lineType = Line.BeginVerbatimBlock "code", content = "" }
        , test "Markdown.classify False begin math block" <|
            \_ ->
                Markdown.classify False "$$"
                    |> Expect.equal { indent = 0, lineType = Line.BeginVerbatimBlock "math", content = "" }
        , test "Markdown.classify False begin quotation block" <|
            \_ ->
                Markdown.classify False ">"
                    |> Expect.equal { indent = 0, lineType = Line.BeginBlock "quotation", content = "" }
        ]
