module Common.CursorTest exposing (..)

import Common.Library.ParserTools as ParserTools
import Common.Syntax as Syntax exposing (Meta, Text(..))
import Common.Text.Configuration as Configuration
import Common.Text.Cursor as Cursor
import Common.Text.Rule as Rule
import Expect exposing (Expectation)
import Set
import Test exposing (..)


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
            "simple text \\foo"
            [ Text [ "simple text " ] { end = 12, id = "0.0", indent = 0, start = 0 }
            , Marked "foo" [] { end = 16, id = "0.1", indent = 0, start = 12 }
            ]
        , testParseLoopStack "(2)"
            "\\foo{bar}"
            [ Marked "foo" [ Text [ "bar" ] { end = 8, id = "0.1", indent = 0, start = 4 } ] { end = 8, id = "0.0", indent = 0, start = 0 }
            ]
        , Test.only <|
            testParseLoopStack "(3)"
                "\\foo{bar}{baz}"
                [ Marked "foo"
                    [ Text [ "baz" ] { end = 14, id = "0.2", indent = 0, start = 9 }
                    , Text [ "bar" ] { end = 9, id = "0.1", indent = 0, start = 4 }
                    ]
                    { end = 14, id = "0.0", indent = 0, start = 0 }
                ]
        , testParseLoopStack "(4)"
            "\\foo{\\bar{baz}}"
            []
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
            [ Text [ "simple text " ] { start = 0, end = 12, indent = 0, id = "0.0" } ]
        , testNextCursorCommittedMiniLaTeX "(6)"
            12
            "simple text \\foo"
            [ Marked "foo" [] { start = 12, end = 16, indent = 0, id = "0.0" } ]
        , testNextCursorStackMiniLaTeX "(7)"
            12
            "simple text \\foo{bar} baz"
            [ Marked "foo" [] { start = 12, end = 16, indent = 0, id = "0.0" } ]
        ]


suiteConfiguration : Test
suiteConfiguration =
    Test.skip <|
        describe "Configuration"
            [ test "MiniLaTeX Configuration, beginSymbols" <|
                \_ ->
                    Configuration.configure Configuration.miniLaTeXExpectations
                        |> .beginSymbols
                        |> Expect.equal [ "\\{code}", "\\{", "{", "$", "`" ]
            , test "MiniLaTeX Configuration, endSymbols" <|
                \_ ->
                    Configuration.configure Configuration.miniLaTeXExpectations
                        |> .endSymbols
                        |> Expect.equal [ "}", "$", "`" ]
            , test "MiniLaTeX Configuration, beginChars" <|
                \_ ->
                    Configuration.configure Configuration.miniLaTeXExpectations
                        |> .beginChars
                        |> Expect.equal [ '\\', '{', '$', '`' ]
            , test "MiniLaTeX Configuration, endChars" <|
                \_ ->
                    Configuration.configure Configuration.miniLaTeXExpectations
                        |> .endChars
                        |> Set.fromList
                        |> Expect.equal ([ '}', '$', '`' ] |> Set.fromList)
            , test "MiniLaTeX Configuration, delimiters" <|
                \_ ->
                    Configuration.configure Configuration.miniLaTeXExpectations
                        |> .delimiters
                        |> Set.fromList
                        |> Expect.equal ([ '\\', '{', '}', '$', '`' ] |> Set.fromList)
            , test "MiniLaTeX Configuration, verbatimChars" <|
                \_ ->
                    Configuration.configure Configuration.miniLaTeXExpectations
                        |> .verbatimChars
                        |> Set.fromList
                        |> Expect.equal ([ '$', '`' ] |> Set.fromList)
            ]
