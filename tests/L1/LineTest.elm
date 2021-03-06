module L1.LineTest exposing (suite)

import Common.BlockParser as BlockParser
import Common.Line as Line
import Common.Syntax as Syntax
import Expect exposing (Expectation)
import Parser exposing (Problem(..))
import Test exposing (..)


classify =
    BlockParser.classify Syntax.L1


intTest label theTest expectedValue =
    test label <|
        \_ ->
            theTest
                |> Expect.equal expectedValue


suite : Test
suite =
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
                    |> Expect.equal (Err [ { col = 1, problem = UnexpectedChar, row = 1 } ])
        , test "classify False empty line" <|
            \_ ->
                classify False ""
                    |> Expect.equal { indent = 0, lineType = Line.BlankLine, content = "" }
        , test "classify False  blank line with 3 leading spaces" <|
            \_ ->
                classify False "   "
                    |> Expect.equal { indent = 3, lineType = Line.BlankLine, content = "  " }
        , test "classify False ordinary line with 3 leading spaces" <|
            \_ ->
                classify False "   ho ho ho!"
                    |> Expect.equal { content = "  ho ho ho!", indent = 3, lineType = Line.OrdinaryLine }
        , test "classify False block" <|
            \_ ->
                classify False "| indent"
                    |> Expect.equal { content = "indent", indent = 0, lineType = Line.BeginBlock Line.RejectFirstLine "indent" }
        , test
            "classify False verbatim block"
          <|
            \_ ->
                classify False "|| math"
                    |> Expect.equal { content = "math", indent = 0, lineType = Line.BeginVerbatimBlock "math" }
        ]
