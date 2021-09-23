module Markdown.CursorTest exposing (..)

import Common.Library.ParserTools as ParserTools
import Common.Syntax as Syntax exposing (Meta, Text(..))
import Common.Text.Cursor as Cursor
import Expect exposing (Expectation)
import Markdown.Rule as Rule
import Set
import Test exposing (..)


markdownParseLoopCommitted : String -> List Text
markdownParseLoopCommitted input =
    Cursor.parseLoop Rule.markdownRules (Cursor.init 0 0 0 input) |> .committed


markdownParseLoop : String -> Cursor.TextCursor
markdownParseLoop input =
    Cursor.parseLoop Rule.markdownRules (Cursor.init 0 0 0 input)


testParseLoopCommitted : String -> String -> List Text -> Test
testParseLoopCommitted label input output =
    test label <| \_ -> Cursor.parseLoop Rule.markdownRules (Cursor.init 0 0 0 input) |> .committed |> List.reverse |> Expect.equal output


stringDataContent : Cursor.Step Cursor.TextCursor Cursor.TextCursor -> String
stringDataContent stepTC =
    mapStepCursor (.stringData >> .content) stepTC


mapStepCursor : (Cursor.TextCursor -> a) -> Cursor.Step Cursor.TextCursor Cursor.TextCursor -> a
mapStepCursor f stepTC =
    case stepTC of
        Cursor.Done tc ->
            f tc

        Cursor.Loop tc ->
            f tc


suiteParseLoop : Test
suiteParseLoop =
    describe "the parseLoop function for Markdown"
        [ Test.only <|
            testParseLoopCommitted "(1)"
                "# Introduction to Chemistry"
                [ Marked "#" [ Text "Introduction to Chemistry" { end = 27, id = "0.1", indent = 0, start = 2 } ] { end = 27, id = "0.0", indent = 0, start = 0 } ]
        , testParseLoopCommitted "(2)"
            "It was *very* bold"
            [ Text "It was " { end = 7, id = "0.0", indent = 0, start = 0 }
            , Marked "*" [ Text "very" { end = 12, id = "0.2", indent = 0, start = 8 } ] { end = 8, id = "0.1", indent = 0, start = 12 }
            , Text " bold" { end = 18, id = "0.4", indent = 0, start = 13 }
            ]
        , testParseLoopCommitted "(3)"
            "Some code: `a := 1`"
            [ Text "Some code: " { end = 11, id = "0.0", indent = 0, start = 0 }, Verbatim "`" "a := 1" { end = 12, id = "0.1", indent = 0, start = 18 } ]
        , testParseLoopCommitted "(4)"
            "Some math: $a^2 = 7$"
            [ Text "Some math: " { end = 11, id = "0.0", indent = 0, start = 0 }
            , Verbatim "$" "a^2 = 7" { end = 12, id = "0.1", indent = 0, start = 19 }
            ]
        ]
