module L1.CursorTest exposing (..)

import Common.Debug exposing (..)
import Common.Syntax as Syntax exposing (Expr(..), Meta)
import Common.Text.Cursor as Cursor
import Expect exposing (Expectation)
import L1.Rule as Rule
import Test exposing (..)


testParseLoopCommitted : String -> String -> List Expr -> Test
testParseLoopCommitted label input output =
    test label <| \_ -> Cursor.parseLoop Rule.rules (Cursor.init 0 0 0 input) |> .committed |> List.reverse |> debug3 "OUTPUT" |> Expect.equal output


suiteParseLoop : Test
suiteParseLoop =
    describe "the parseLoop function for L1"
        [ testParseLoopCommitted "(0)"
            "AAA BBB"
            [ Text "AAA BBB " { end = 7, id = "0.0", indent = 0, begin = 0 } ]
        , testParseLoopCommitted "(1)"
            "[i AAA BBB]"
            [ Expr "italic"
                [ Text "AAA " { end = 10, id = "0.1", indent = 0, begin = 3 }
                , Text "BBB " { end = 10, id = "0.1", indent = 0, begin = 3 }
                ]
                { end = 3, id = "0.0", indent = 0, begin = 0 }
            ]
        , testParseLoopCommitted "(2)"
            "This is [i italic]"
            [ Text "This is " { end = 8, id = "0.0", indent = 0, begin = 0 }
            , Expr "italic" [ Text "italic " { end = 17, id = "0.2", indent = 0, begin = 11 } ] { end = 11, id = "0.1", indent = 0, begin = 8 }
            ]
        , testParseLoopCommitted "(3)"
            "This is [i italic] ho ho ho!"
            [ Text "This is " { end = 8, id = "0.0", indent = 0, begin = 0 }
            , Expr "italic" [ Text "italic " { end = 17, id = "0.2", indent = 0, begin = 11 } ] { end = 11, id = "0.1", indent = 0, begin = 8 }
            , Text " " { end = 0, id = "1.2", indent = 0, begin = 0 }
            , Text "ho ho ho! " { end = 28, id = "0.5", indent = 0, begin = 19 }
            ]
        , testParseLoopCommitted "(4)"
            "[b [red flowers]]"
            [ Expr "strong" [ Expr "red" [ Text "flowers " { end = 15, id = "0.2", indent = 0, begin = 8 } ] { end = 15, id = "0.1", indent = 0, begin = 3 } ] { end = 15, id = "0.0", indent = 0, begin = 0 } ]
        , testParseLoopCommitted "(5)"
            "[link label url]"
            [ Expr "link"
                [ Text "label " { end = 15, id = "0.1", indent = 0, begin = 6 }
                , Text "url " { end = 15, id = "0.1", indent = 0, begin = 6 }
                ]
                { end = 6, id = "0.0", indent = 0, begin = 0 }
            ]
        , testParseLoopCommitted "(6)"
            "[i AAA [b BBB]] "
            [ Expr "italic"
                [ Text "AAA " { end = 7, id = "0.1", indent = 0, begin = 3 }
                , Expr "strong" [ Text "BBB " { end = 13, id = "0.3", indent = 0, begin = 10 } ] { end = 13, id = "0.2", indent = 0, begin = 7 }
                ]
                { end = 13, id = "0.0", indent = 0, begin = 0 }
            , Text " " { end = 0, id = "1.2", indent = 0, begin = 0 }
            ]
        , testParseLoopCommitted "(7)"
            "[i Samson is very [b tall]]"
            [ Expr "italic"
                [ Text "Samson " { end = 18, id = "0.1", indent = 0, begin = 3 }
                , Text "is " { end = 18, id = "0.1", indent = 0, begin = 3 }
                , Text "very " { end = 18, id = "0.1", indent = 0, begin = 3 }
                , Expr "strong" [ Text "tall " { end = 25, id = "0.3", indent = 0, begin = 21 } ] { end = 25, id = "0.2", indent = 0, begin = 18 }
                ]
                { end = 25, id = "0.0", indent = 0, begin = 0 }
            ]
        , testParseLoopCommitted "(8)" "`a b c`" [ Verbatim "code" "a b c" { end = 8, id = "0.0", indent = 0, begin = 0 } ]
        , testParseLoopCommitted "(9)"
            "`a b c` DEF"
            [ Verbatim "code" "a b c" { end = 8, id = "0.0", indent = 0, begin = 0 }
            , Text "DEF " { end = 11, id = "0.1", indent = 0, begin = 8 }
            ]
        , testParseLoopCommitted "(10)"
            "[link ABC DEF] GHI"
            [ Expr "link" [ Text "ABC " { end = 13, id = "0.1", indent = 0, begin = 6 }, Text "DEF " { end = 13, id = "0.1", indent = 0, begin = 6 } ] { end = 6, id = "0.0", indent = 0, begin = 0 }
            , Text " " { end = 0, id = "1.2", indent = 0, begin = 0 }
            , Text "GHI " { end = 18, id = "0.4", indent = 0, begin = 15 }
            ]
        , testParseLoopCommitted
            "(11)"
            "[link `a b c` DEF]"
            [ Expr "link"
                [ Verbatim "code" "a b c" { end = 14, id = "0.1", indent = 0, begin = 6 }
                , Text "DEF " { end = 17, id = "0.2", indent = 0, begin = 14 }
                ]
                { end = 6, id = "0.0", indent = 0, begin = 0 }
            ]
        , testParseLoopCommitted
            "(12)"
            " $e^{ikx}$"
            [ Verbatim "$" "e^{ikx} " { end = 2, id = "0.1", indent = 0, begin = 9 } ]
        ]
