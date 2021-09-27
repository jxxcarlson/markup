module L1.CursorTest exposing (..)

import Common.Debug exposing (..)
import Common.Syntax as Syntax exposing (Meta, Text(..))
import Common.Text.Cursor as Cursor
import Expect exposing (Expectation)
import L1.Rule as Rule
import Test exposing (..)


testParseLoopCommitted : String -> String -> List Text -> Test
testParseLoopCommitted label input output =
    test label <| \_ -> Cursor.parseLoop Rule.rules (Cursor.init 0 0 0 input) |> .committed |> List.reverse |> debug3 "OUTPUT" |> Expect.equal output


suiteParseLoop : Test
suiteParseLoop =
    describe "the parseLoop function for L1"
        [ testParseLoopCommitted "(0)"
            "AAA BBB"
            [ Text "AAA BBB " { end = 7, id = "0.0", indent = 0, start = 0 } ]
        , testParseLoopCommitted "(1)"
            "[i AAA BBB]"
            [ Marked "italic"
                [ Text "AAA " { end = 10, id = "0.1", indent = 0, start = 3 }
                , Text "BBB " { end = 10, id = "0.1", indent = 0, start = 3 }
                ]
                { end = 3, id = "0.0", indent = 0, start = 0 }
            ]
        , testParseLoopCommitted "(2)"
            "This is [i italic]"
            [ Text "This is " { end = 8, id = "0.0", indent = 0, start = 0 }
            , Marked "italic" [ Text "italic " { end = 17, id = "0.2", indent = 0, start = 11 } ] { end = 11, id = "0.1", indent = 0, start = 8 }
            ]
        , testParseLoopCommitted "(3)"
            "This is [i italic] ho ho ho!"
            [ Text "This is " { end = 8, id = "0.0", indent = 0, start = 0 }
            , Marked "italic" [ Text "italic " { end = 17, id = "0.2", indent = 0, start = 11 } ] { end = 11, id = "0.1", indent = 0, start = 8 }
            , Text " " { end = 0, id = "1.2", indent = 0, start = 0 }
            , Text "ho ho ho! " { end = 28, id = "0.5", indent = 0, start = 19 }
            ]
        , testParseLoopCommitted "(4)"
            "[b [red flowers]]"
            [ Marked "strong" [ Marked "red" [ Text "flowers " { end = 15, id = "0.2", indent = 0, start = 8 } ] { end = 15, id = "0.1", indent = 0, start = 3 } ] { end = 15, id = "0.0", indent = 0, start = 0 } ]
        , testParseLoopCommitted "(5)"
            "[link label url]"
            [ Marked "link"
                [ Text "label " { end = 15, id = "0.1", indent = 0, start = 6 }
                , Text "url " { end = 15, id = "0.1", indent = 0, start = 6 }
                ]
                { end = 6, id = "0.0", indent = 0, start = 0 }
            ]
        , testParseLoopCommitted "(6)"
            "[i AAA [b BBB]] "
            [ Marked "italic"
                [ Text "AAA " { end = 7, id = "0.1", indent = 0, start = 3 }
                , Marked "strong" [ Text "BBB " { end = 13, id = "0.3", indent = 0, start = 10 } ] { end = 13, id = "0.2", indent = 0, start = 7 }
                ]
                { end = 13, id = "0.0", indent = 0, start = 0 }
            , Text " " { end = 0, id = "1.2", indent = 0, start = 0 }
            ]
        ]
