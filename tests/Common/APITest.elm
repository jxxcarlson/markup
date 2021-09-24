module Common.APITest exposing (..)

import Common.BasicSyntax as Syntax exposing (BasicBlock(..))
import Common.Syntax exposing (Language(..), Text(..), TextBlock(..))
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
        [ Test.only <|
            testAPI Markdown
                "one\ntwo"
                [ TBParagraph [ Text "one" { end = 3, id = "0.0", indent = 0, start = 0 }, Text "two" { end = 3, id = "0.0", indent = 0, start = 0 } ] { end = 0, id = "1.0", indent = 0, start = 0 } ]
        , testAPI Markdown
            "```\n   one\n   two"
            -- TODO: incorrect position information
            [ TBVerbatimBlock "code" [ "   one", "   two" ] { end = 1, id = "1.1", indent = 3, start = 1 } ]
        ]
