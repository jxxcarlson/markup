module Common.Line exposing (LineType(..), countLeadingSpaces, emptyLineParser, ordinaryLineParser)

import Parser exposing ((|.), (|=), Parser)


type LineType
    = OrdinaryLine
    | BlankLine
    | BeginBlock String
    | EndBlock String
    | BeginVerbatimBlock String
    | EndVerbatimBlock String
    | Problem String


countLeadingSpaces : String -> Int
countLeadingSpaces str =
    case Parser.run leadingBlanksParser str of
        Ok k ->
            k

        Err _ ->
            0


leadingBlanksParser : Parser Int
leadingBlanksParser =
    (Parser.getChompedString <|
        Parser.succeed ()
            |. Parser.chompWhile (\c -> c == ' ')
    )
        |> Parser.map String.length


emptyLineParser : Parser LineType
emptyLineParser =
    Parser.end |> Parser.map (\_ -> BlankLine)


ordinaryLineParser : List Char -> Parser LineType
ordinaryLineParser blockStartChars =
    Parser.chompIf (\c -> not (List.member c blockStartChars)) |> Parser.map (\_ -> OrdinaryLine)
