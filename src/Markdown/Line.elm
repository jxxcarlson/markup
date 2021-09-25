module Markdown.Line exposing (lineType)

import Common.Debug exposing (debug3)
import Common.Library.ParserTools as ParserTools
import Common.Line as Line
import Parser exposing ((|.), (|=), Parser)


lineType : String -> Line.LineType
lineType str =
    let
        _ =
            debug3 "lineType, line" str
    in
    case Parser.run lineTypeParser str of
        Ok type_ ->
            type_

        Err _ ->
            Line.Problem "unrecognized type"


lineTypeParser =
    Parser.oneOf
        [ beginCodeBlockParser
        , beginMathBlockParser
        , beginItemParser
        , beginQuotationBlockParser
        , Line.ordinaryLineParser []
        , Line.emptyLineParser
        ]


beginItemParser : Parser Line.LineType
beginItemParser =
    (Parser.succeed String.slice
        |. Parser.symbol "-"
    )
        |> Parser.map (\_ -> Line.BeginBlock Line.AcceptFirstLine "item")


beginMathBlockParser : Parser Line.LineType
beginMathBlockParser =
    (Parser.succeed String.slice
        |. Parser.symbol "$$"
    )
        |> Parser.map (\_ -> Line.BeginVerbatimBlock "math")


beginCodeBlockParser : Parser Line.LineType
beginCodeBlockParser =
    (Parser.succeed String.slice
        |. Parser.symbol "```"
    )
        |> Parser.map (\_ -> Line.BeginVerbatimBlock "code")


beginQuotationBlockParser : Parser Line.LineType
beginQuotationBlockParser =
    (Parser.succeed String.slice
        |. Parser.symbol ">"
    )
        |> Parser.map (\_ -> Line.BeginBlock Line.AcceptFirstLine "quotation")
