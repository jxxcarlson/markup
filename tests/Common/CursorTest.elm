module Common.CursorTest exposing (..)

import Common.BasicSyntax as Syntax exposing (BasicBlock(..))
import Common.Library.ParserTools as ParserTools
import Common.Text.Configuration as Configuration
import Common.Text.Cursor as Cursor
import Common.Text.Rule as Rule
import Expect exposing (Expectation)
import Set
import Test exposing (..)


testAdvanceMiniLaTeX : String -> Int -> String -> String -> Test
testAdvanceMiniLaTeX label scanPoint input output =
    test label <| \_ -> Cursor.advance Configuration.miniLaTeXConfig (Cursor.init scanPoint input) |> .stringData >> .content |> Expect.equal output


testNextCursorMiniLaTeX : String -> Int -> String -> String -> Test
testNextCursorMiniLaTeX label scanPoint input output =
    test label <| \_ -> Cursor.nextCursor Rule.miniLaTeXRules (Cursor.init scanPoint input) |> stringDataContent |> Expect.equal output


stringDataContent : Cursor.Step Cursor.TextCursor Cursor.TextCursor -> String
stringDataContent stepTC =
    case stepTC of
        Cursor.Done tc ->
            tc.stringData.content

        Cursor.Loop tc ->
            tc.stringData.content


suiteMiniLaTeXNextCursor : Test
suiteMiniLaTeXNextCursor =
    Test.only <|
        describe "the nextCursor function for MiniLaTeX"
            [ testNextCursorMiniLaTeX "(1)" 0 "simple text \\foo" "simple text "
            , testNextCursorMiniLaTeX "(2)" 12 "simple text \\foo" "\\foo"
            , testNextCursorMiniLaTeX "(3)" 12 "simple text \\foo ha ha ha!" "\\foo"
            , testNextCursorMiniLaTeX "(4)" 16 "simple text \\foo ha ha ha!" " ha ha ha!"
            ]


suiteAdvanceMiniLaTeX : Test
suiteAdvanceMiniLaTeX =
    describe "the advance function for MiniLaTeX"
        [ testAdvanceMiniLaTeX "(1)" 0 "simple text \\{foo}" "simple text "
        , testAdvanceMiniLaTeX "(2)" 14 "simple text \\{foo}" "foo"
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
