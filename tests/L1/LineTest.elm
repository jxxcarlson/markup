module L1.LineTest exposing (suite)

import Common.Line as Line
import Expect exposing (Expectation)
import L1.Line as L1
import Parser
import Test exposing (..)


intTest label theTest expectedValue =
    test label <|
        \_ ->
            theTest
                |> Expect.equal expectedValue


suite : Test
suite =
    Test.skip <|
        describe "The Line modules for L1"
            [ intTest "countLeadingSpaces" (Line.countLeadingSpaces "   abc") 3
            , test "blankLineParser" <|
                \_ ->
                    Parser.run Line.emptyLineParser ""
                        |> Expect.equal (Ok Line.BlankLine)
            , test "ordinaryLineParser, succeed" <|
                \_ ->
                    Parser.run (Line.ordinaryLineParser [ '|' ]) "ho ho ho!"
                        |> Expect.equal (Ok Line.OrdinaryLine)
            , test "ordinaryLineParser, fail" <|
                \_ ->
                    Parser.run (Line.ordinaryLineParser [ '|' ]) "| ho ho ho!"
                        |> Expect.notEqual (Ok Line.OrdinaryLine)
            , test "L1.classify empty line" <|
                \_ ->
                    L1.classify ""
                        |> Expect.equal { indent = 0, lineType = Line.BlankLine, content = "   " }
            , test "L1.classify  blank line with 3 leading spaces" <|
                \_ ->
                    L1.classify "   "
                        |> Expect.equal { indent = 3, lineType = Line.BlankLine, content = "   " }
            , test "L1.classify ordinary line with 3 leading spaces" <|
                \_ ->
                    L1.classify "   ho ho ho!"
                        |> Expect.equal { indent = 3, lineType = Line.OrdinaryLine, content = "   " }
            , test "L1.classify block" <|
                \_ ->
                    L1.classify "| indent"
                        |> Expect.equal { indent = 0, lineType = Line.BeginBlock "indent", content = "   " }
            , test "L1.classify verbatim block" <|
                \_ ->
                    L1.classify "|| math"
                        |> Expect.equal { indent = 0, lineType = Line.BeginVerbatimBlock "math", content = "   " }
            ]
