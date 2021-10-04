module Common.Text.Error exposing (Context(..), Problem(..), heading)


type Problem
    = EndOfInput
    | ExpectingBackTick
    | UnHandledError Int


heading : Problem -> String
heading problem =
    case problem of
        _ ->
            "Error in"


type Context
    = TextExpression
