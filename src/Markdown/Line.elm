module Markdown.Line exposing (classify)

import Common.Line as Line
import Parser exposing ((|.), (|=), Parser)


classify : String -> { indent : Int, lineType : Line.LineType }
classify str =
    let
        leadingSpaces =
            Line.countLeadingSpaces str
    in
    { indent = leadingSpaces, lineType = lineType (String.dropLeft leadingSpaces str) }


lineType str =
    case Parser.run lineTypeParser str of
        Ok type_ ->
            type_

        Err _ ->
            Line.Problem "unrecognized type"


lineTypeParser =
    Parser.oneOf [ beginCodeBlockParser, beginMathBlockParser, beginQuotationBlockParser, Line.ordinaryLineParser [], Line.emptyLineParser ]


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
        |> Parser.map (\_ -> Line.BeginBlock "quotation")
