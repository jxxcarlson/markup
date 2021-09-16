module L1.Line exposing (classify)

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
    Parser.oneOf [ Line.ordinaryLineParser [ '|' ], Line.emptyLineParser, beginVerbatimBlockParser, beginBlockParser ]


beginBlockParser : Parser Line.LineType
beginBlockParser =
    (Parser.succeed String.slice
        |. Parser.symbol "|"
        |. Parser.chompIf (\c -> c == ' ')
        |= Parser.getOffset
        |. Parser.chompWhile (\c -> c /= ' ')
        |= Parser.getOffset
        |= Parser.getSource
    )
        |> Parser.map (\s -> Line.BeginBlock s)


beginVerbatimBlockParser : Parser Line.LineType
beginVerbatimBlockParser =
    (Parser.succeed String.slice
        |. Parser.symbol "||"
        |. Parser.chompIf (\c -> c == ' ')
        |= Parser.getOffset
        |. Parser.chompWhile (\c -> c /= ' ')
        |= Parser.getOffset
        |= Parser.getSource
    )
        |> Parser.map (\s -> Line.BeginVerbatimBlock s)
