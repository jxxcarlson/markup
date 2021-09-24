module MiniLaTeX.Line exposing (lineType)

import Common.Library.ParserTools as ParserTools
import Common.Line as Line
import Parser exposing ((|.), (|=), Parser)


lineType : String -> Line.LineType
lineType str =
    case Parser.run lineTypeParser str of
        Ok type_ ->
            type_

        Err _ ->
            Line.Problem "unrecognized type"


lineTypeParser =
    Parser.oneOf [ beginCodeBlockParser, beginBlockParser, endBlockParser, beginMathBlockParser, Line.ordinaryLineParser [], Line.emptyLineParser ]


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


beginBlockParser : Parser Line.LineType
beginBlockParser =
    (Parser.succeed String.slice
        |. Parser.symbol "\\begin{"
        |= Parser.getOffset
        |. Parser.chompWhile (\c -> c /= '}')
        |= Parser.getOffset
        |= Parser.getSource
    )
        |> Parser.map (\s -> Line.BeginBlock s)


endBlockParser : Parser Line.LineType
endBlockParser =
    (Parser.succeed String.slice
        |. Parser.symbol "\\end{"
        |= Parser.getOffset
        |. Parser.chompWhile (\c -> c /= '}')
        |= Parser.getOffset
        |= Parser.getSource
    )
        |> Parser.map (\s -> Line.EndBlock s)
