module MiniLaTeX.CursorTest exposing (..)

import Common.Library.ParserTools as ParserTools
import Common.Syntax as Syntax exposing (Expr(..), Meta)
import Common.Text.Cursor as Cursor
import Expect exposing (Expectation)
import MiniLaTeX.Rule as Rule
import Set
import Test exposing (..)


testParseLoopCommitted : String -> String -> List Expr -> Test
testParseLoopCommitted label input output =
    test label <| \_ -> Cursor.parseLoop Rule.rules (Cursor.init 0 0 0 input) |> .committed |> List.reverse |> Expect.equal output


suiteParseLoop : Test
suiteParseLoop =
    describe "the parseLoop function for MiniLaTeX"
        [ testParseLoopCommitted "(1)"
            "abc \\foo"
            [ Text "abc " { end = 4, id = "0.0", indent = 0, begin = 0 }
            , Expr "foo" [] { end = 8, id = "0.1", indent = 0, begin = 4 }
            ]
        , testParseLoopCommitted "(2)"
            "\\foo{bar}"
            [ Expr "foo" [ Text "bar" { end = 8, id = "0.2", indent = 0, begin = 5 } ] { end = 8, id = "0.0", indent = 0, begin = 0 } ]
        , testParseLoopCommitted "(3)"
            "\\foo{bar}{baz}"
            [ Expr "foo"
                [ Text "bar" { end = 8, id = "0.2", indent = 0, begin = 5 }
                , Text "baz" { end = 13, id = "0.5", indent = 0, begin = 10 }
                ]
                { end = 13, id = "0.0", indent = 0, begin = 0 }
            ]
        , testParseLoopCommitted "(4)"
            "\\foo{\\bar{baz}}"
            [ Expr "foo" [ Expr "bar" [ Text "baz" { end = 13, id = "0.4", indent = 0, begin = 10 } ] { end = 13, id = "0.2", indent = 0, begin = 5 } ] { end = 13, id = "0.0", indent = 0, begin = 0 } ]
        , testParseLoopCommitted "(5)"
            "very \\strong{bold} move"
            [ Text "very " { end = 5, id = "0.0", indent = 0, begin = 0 }
            , Expr "strong" [ Text "bold" { end = 17, id = "0.3", indent = 0, begin = 13 } ] { end = 17, id = "0.1", indent = 0, begin = 5 }
            , Text " " { end = 0, id = "1.2", indent = 0, begin = 0 }
            , Text "move" { end = 23, id = "0.6", indent = 0, begin = 19 }
            ]
        , testParseLoopCommitted "(6)"
            "\\link{NYT}{https://nytimes.com} "
            [ Expr "link"
                [ Text "NYT" { end = 9, id = "0.2", indent = 0, begin = 6 }
                , Text "https://nytimes.com" { end = 30, id = "0.5", indent = 0, begin = 11 }
                ]
                { end = 30, id = "0.0", indent = 0, begin = 0 }
            , Text " " { end = 0, id = "1.2", indent = 0, begin = 0 }
            ]
        , testParseLoopCommitted "(7)"
            "\\link{NYT}{https://nytimes.com}"
            [ Expr "link"
                [ Text "NYT" { end = 9, id = "0.2", indent = 0, begin = 6 }
                , Text "https://nytimes.com" { end = 30, id = "0.5", indent = 0, begin = 11 }
                ]
                { end = 30, id = "0.0", indent = 0, begin = 0 }
            ]
        , testParseLoopCommitted
            "(8)"
            "$e^{ikx}$"
            [ Verbatim "$" "e^{ikx}" { end = 9, id = "0.0", indent = 0, begin = 0 } ]
        ]
