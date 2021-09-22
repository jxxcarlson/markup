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
        , test "Markdown.classify empty line" <|
            \_ ->
                Markdown.classify ""
                    |> Expect.equal { indent = 0, lineType = Line.BlankLine, content = "" }
        , test "Markdown.classify  blank line with 3 leading spaces" <|
            \_ ->
                Markdown.classify "   "
                    |> Expect.equal { indent = 3, lineType = Line.BlankLine, content = "   " }
        , test "Markdown.classify ordinary line with 3 leading spaces" <|
            \_ ->
                Markdown.classify "   ho ho ho!"
                    |> Expect.equal { indent = 3, lineType = Line.OrdinaryLine, content = "   ho ho ho!" }
        , test "Markdown.classify begin code block" <|
            \_ ->
                Markdown.classify "```"
                    |> Expect.equal { indent = 0, lineType = Line.BeginVerbatimBlock "code", content = "" }
        , test "Markdown.classify begin math block" <|
            \_ ->
                Markdown.classify "$$"
                    |> Expect.equal { indent = 0, lineType = Line.BeginVerbatimBlock "math", content = "" }
        , test "Markdown.classify begin quotation block" <|
            \_ ->
                Markdown.classify ">"
                    |> Expect.equal { indent = 0, lineType = Line.BeginBlock "quotation", content = "" }
        ]
