module Markdown.CursorTest exposing (..)

import Common.Syntax as Syntax exposing (Meta, Text(..))
import Common.Text.Cursor as Cursor
import Expect exposing (Expectation)
import Markdown.Rule as Rule
import Test exposing (..)


testParseLoopCommitted : String -> String -> List Text -> Test
testParseLoopCommitted label input output =
    test label <| \_ -> Cursor.parseLoop Rule.rules (Cursor.init 0 0 0 input) |> .committed |> List.reverse |> Expect.equal output


suiteParseLoop : Test
suiteParseLoop =
    describe "the parseLoop function for Markdown"
        [ testParseLoopCommitted "(1)"
            "# Introduction to Chemistry"
            [ Marked "title" [ Text "Introduction to Chemistry" { end = 27, id = "0.1", indent = 0, start = 2 } ] { end = 27, id = "0.0", indent = 0, start = 0 } ]
        , testParseLoopCommitted "(2)"
            "It was *very* bold"
            [ Text "It was " { end = 7, id = "0.0", indent = 0, start = 0 }
            , Marked "strong" [ Text "very" { end = 12, id = "0.2", indent = 0, start = 8 } ] { end = 8, id = "0.1", indent = 0, start = 12 }
            , Text " " { end = 0, id = "1.2", indent = 0, start = 0 }
            , Text "bold" { end = 18, id = "0.5", indent = 0, start = 14 }
            ]
        , testParseLoopCommitted "(3)"
            "Some code: `a := 1`"
            [ Text "Some code: " { end = 11, id = "0.0", indent = 0, start = 0 }, Verbatim "`" "a := 1" { end = 12, id = "0.1", indent = 0, start = 18 } ]
        , testParseLoopCommitted "(4)"
            "Some math: $a^2 = 7$"
            [ Text "Some math: " { end = 11, id = "0.0", indent = 0, start = 0 }
            , Verbatim "$" "a^2 = 7" { end = 12, id = "0.1", indent = 0, start = 19 }
            ]
        , testParseLoopCommitted
            "(5)"
            " $e^{ikx}$"
            [ Verbatim "$" "e^{ikx}" { end = 2, id = "0.1", indent = 0, start = 9 } ]
        ]
