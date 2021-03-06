module Common.APITest exposing (..)

import Common.BasicSyntax as Syntax exposing (BasicBlock(..))
import Common.Syntax exposing (Expr(..), Language(..), TextBlock(..))
import Expect exposing (Expectation)
import Markup.API as API
import Test exposing (..)


parse lang str =
    API.parse lang 1 (String.lines str)


testAPI : Language -> String -> List TextBlock -> Test
testAPI language input output =
    test input <|
        \_ ->
            parse language input
                |> Expect.equal output


suite : Test
suite =
    describe "The API"
        [ testAPI Markdown
            "one\ntwo"
            [ TBParagraph
                [ Text "one\n" { end = 4, id = "0.0", indent = 0, begin = 0 }
                , Text "two\n" { end = 4, id = "0.0", indent = 0, begin = 0 }
                ]
                { end = 0, id = "1.0", indent = 0, begin = 0 }
            ]
        , testAPI Markdown
            "one\nthree\n"
            [ TBParagraph [ Text "one\n" { end = 4, id = "0.0", indent = 0, begin = 0 }, Text "three\n" { end = 6, id = "0.0", indent = 0, begin = 0 } ] { end = 0, id = "1.0", indent = 0, begin = 0 }
            , TBParagraph [] { end = 0, id = "1.2", indent = 0, begin = 0 }
            ]
        , testAPI Markdown
            "```\n   one\n   two"
            -- TODO: incorrect position information
            [ TBVerbatimBlock "code" [ "   one", "   two" ] { end = 1, id = "1.1", indent = 3, begin = 1 } ]
        , testAPI Markdown
            "```\n   aaa\n      bbb\n   cccc"
            [ TBVerbatimBlock "code" [ "   aaa", "      bbb", "   cccc" ] { end = 1, id = "1.1", indent = 3, begin = 1 } ]
        , testAPI MiniLaTeX
            "\\begin{mathmacro}\n   \\newcommand{\\bra}[0]{\\langle}\n   \\newcommand{\\ket}[0]{\\rangle}\n\\end{mathmacro}"
            [ TBVerbatimBlock "mathmacro" [ "   \\newcommand{\\bra}[0]{\\langle}", "   \\newcommand{\\ket}[0]{\\rangle}" ] { end = 0, id = "1.0", indent = 0, begin = 0 } ]
        ]



--   testParseLoopCommitted
--       "(9)"
--       "\\begin{mathmacro}\n   \\ewcommand{\\bra}[0]{\\langle}\n\\end{mathmacro}"
--       [  ]
--, Test.only <|
--           testParseLoopCommitted
--               "(10)"
--               "\\begin{equation}\n    \\int_0^1 x^n dx\n\\end{equation}"
--               [  ]
