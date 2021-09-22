module MiniLaTeX.CursorTest exposing (..)

import Common.Library.ParserTools as ParserTools
import Common.Syntax as Syntax exposing (Meta, Text(..))
import Common.Text.Cursor as Cursor
import Expect exposing (Expectation)
import MiniLaTeX.Rule as Rule
import Set
import Test exposing (..)


miniLaTeXParseLoopCommitted : String -> List Text
miniLaTeXParseLoopCommitted input =
    Cursor.parseLoop Rule.miniLaTeXRules (Cursor.init 0 0 0 input) |> .committed


miniLaTeXParseLoop : String -> Cursor.TextCursor
miniLaTeXParseLoop input =
    Cursor.parseLoop Rule.miniLaTeXRules (Cursor.init 0 0 0 input)


testNextCursorMiniLaTeX : String -> Int -> String -> String -> Test
testNextCursorMiniLaTeX label scanPoint input output =
    test label <| \_ -> Cursor.nextCursor Rule.miniLaTeXRules (Cursor.init 0 0 scanPoint input) |> stringDataContent |> Expect.equal output


testNextCursorCommittedMiniLaTeX : String -> Int -> String -> List Text -> Test
testNextCursorCommittedMiniLaTeX label scanPoint input output =
    test label <| \_ -> Cursor.nextCursor Rule.miniLaTeXRules (Cursor.init 0 0 scanPoint input) |> mapStepCursor .committed |> Expect.equal output


testNextCursorStackMiniLaTeX : String -> Int -> String -> List Text -> Test
testNextCursorStackMiniLaTeX label scanPoint input output =
    test label <| \_ -> Cursor.nextCursor Rule.miniLaTeXRules (Cursor.init 0 0 scanPoint input) |> mapStepCursor .stack |> Expect.equal output


testParseLoopCommitted : String -> String -> List Text -> Test
testParseLoopCommitted label input output =
    test label <| \_ -> Cursor.parseLoop Rule.miniLaTeXRules (Cursor.init 0 0 0 input) |> .committed |> List.reverse |> Expect.equal output


testParseLoopStack : String -> String -> List Text -> Test
testParseLoopStack label input output =
    test label <| \_ -> Cursor.parseLoop Rule.miniLaTeXRules (Cursor.init 0 0 0 input) |> .stack |> Expect.equal output



-- |> .parsed |> Expect.equal output


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
    describe "the parseLoop function for MiniLaTeX"
        [ testParseLoopCommitted "(1)"
            "abc \\foo"
            [ Text "abc " { end = 4, id = "0.0", indent = 0, start = 0 }
            , Marked "foo" [] { end = 8, id = "0.1", indent = 0, start = 4 }
            ]
        , Test.only <|
            testParseLoopCommitted "(2)"
                "\\foo{bar}"
                [ Marked "foo" [ Text "bar" { end = 8, id = "0.2", indent = 0, start = 5 } ] { end = 8, id = "0.0", indent = 0, start = 0 } ]
        , testParseLoopCommitted "(3)"
            "\\foo{bar}{baz}"
            [ Marked "foo"
                [ Text "baz" { end = 13, id = "0.5", indent = 0, start = 10 }
                , Text "bar" { end = 8, id = "0.2", indent = 0, start = 5 }
                ]
                { end = 13, id = "0.0", indent = 0, start = 0 }
            ]
        , testParseLoopCommitted "(4)"
            "\\foo{\\bar{baz}}"
            [ Marked "foo" [ Marked "bar" [ Text "baz" { end = 13, id = "0.4", indent = 0, start = 10 } ] { end = 13, id = "0.2", indent = 0, start = 5 } ] { end = 13, id = "0.0", indent = 0, start = 0 } ]
        , testParseLoopCommitted "(6)"
            "very \\strong{bold} move"
            [ Text "very " { end = 5, id = "0.0", indent = 0, start = 0 }
            , Marked "strong" [ Text "bold" { end = 17, id = "0.3", indent = 0, start = 13 } ] { end = 17, id = "0.1", indent = 0, start = 5 }
            , Text " move" { end = 23, id = "0.5", indent = 0, start = 18 }
            ]
        ]


suiteMiniLaTeXNextCursor : Test
suiteMiniLaTeXNextCursor =
    describe "the nextCursor function for MiniLaTeX"
        [ testNextCursorMiniLaTeX "(1)" 0 "simple text \\foo" "simple text "
        , testNextCursorMiniLaTeX "(2)" 12 "simple text \\foo" "\\foo"
        , testNextCursorMiniLaTeX "(3)" 12 "simple text \\foo ha ha ha!" "\\foo"
        , testNextCursorMiniLaTeX "(4)" 16 "simple text \\foo ha ha ha!" " ha ha ha!"
        , testNextCursorCommittedMiniLaTeX "(5)"
            0
            "simple text \\foo"
            [ Text "simple text " { start = 0, end = 12, indent = 0, id = "0.0" } ]
        , testNextCursorCommittedMiniLaTeX "(6)"
            12
            "simple text \\foo"
            [ Marked "foo" [] { start = 12, end = 16, indent = 0, id = "0.0" } ]
        , testNextCursorStackMiniLaTeX "(7)"
            12
            "simple text \\foo{bar} baz"
            [ Marked "foo" [] { start = 12, end = 16, indent = 0, id = "0.0" } ]
        ]
