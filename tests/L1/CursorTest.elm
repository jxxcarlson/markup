module L1.CursorTest exposing (..)

import Common.Syntax as Syntax exposing (Meta, Text(..))
import Common.Text.Cursor as Cursor
import Expect exposing (Expectation)
import L1.Rule as Rule
import Test exposing (..)


testParseLoopCommitted : String -> String -> List Text -> Test
testParseLoopCommitted label input output =
    test label <| \_ -> Cursor.parseLoop Rule.rules (Cursor.init 0 0 0 input) |> .committed |> List.reverse |> Expect.equal output


suiteParseLoop : Test
suiteParseLoop =
    describe "the parseLoop function for L1"
        [ testParseLoopCommitted "(1)"
            "This is [i italic]"
            []
        ]
