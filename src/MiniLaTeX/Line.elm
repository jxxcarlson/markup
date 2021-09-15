module MiniLaTeX.Line exposing (classify)

import Line
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
    Parser.oneOf [ beginBlockParser, endBlockParser, Line.ordinaryLineParser [], Line.emptyLineParser ]


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
