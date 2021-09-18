module Common.Text.Cursor exposing (..)

import Common.Syntax as Syntax exposing (Text(..))


type alias TextCursor =
    { scanPoint : Int
    , source : String
    , parsed : List Text
    , stack : List StackItem
    }


type alias StackItem =
    { t : Text }


loop : state -> (state -> Step state a) -> a
loop s nextState =
    case nextState s of
        Loop s_ ->
            loop s_ nextState

        Done b ->
            b


type Step state a
    = Loop state
    | Done a
