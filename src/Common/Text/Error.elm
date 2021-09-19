module Common.Text.Error exposing (Context(..), Problem(..), heading)


type Problem
    = ExpectingLeftBracket
    | ExpectingRightBracket
    | ExpectingDollarSign
    | EndOfInput
    | ExpectingEscape
    | ExpectingHashMark
    | ExpectingSpace
    | ExpectingColon
    | ExpectingQuoteMark
    | ExpectingPipe
    | ExpectingBackTick
    | UnHandledError Int
    | NoError


heading : Problem -> String
heading problem =
    case problem of
        ExpectingRightBracket ->
            "Missing right bracket?"

        ExpectingLeftBracket ->
            "Missing left bracket?"

        _ ->
            "Error in"


type Context
    = CElement
    | CArgs
    | CBody
    | CArgsAndBody
    | TextExpression
