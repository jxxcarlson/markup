module L1.BlockParser exposing (run, runFromString)

import Common.BlockParser as BP exposing (State, Step(..), loop)
import Common.Debug exposing (debug1, debug2, debug3)
import Common.Line as Line exposing (LineType(..))
import Common.Syntax exposing (Block(..), BlockM, BlockType(..))
import L1.Line as Line


runFromString : Int -> String -> State
runFromString k str =
    run k (String.lines str)


run : Int -> List String -> State
run generation input =
    BP.loop (BP.initialState generation input) (BP.nextState nextStateAux)


nextStateAux : String -> State -> State
nextStateAux line state =
    let
        lineType =
            Line.classify line

        indent =
            lineType.indent
    in
    case lineType.lineType of
        BeginBlock s ->
            if BP.level indent <= BP.blockLevelOfStackTop state.stack then
                { state | indent = indent } |> BP.reduceStack |> BP.shift (Block s [])

            else
                { state | indent = indent } |> BP.shift (Block s [])

        BeginVerbatimBlock s ->
            if BP.level indent <= BP.blockLevelOfStackTop state.stack then
                { state | indent = indent } |> BP.reduceStack |> BP.shift (VerbatimBlock s [])

            else
                { state | indent = indent } |> BP.shift (VerbatimBlock s [])

        OrdinaryLine ->
            state |> handleOrdinaryLine indent line

        BlankLine ->
            handleBlankLine indent state

        EndBlock s ->
            -- TODO: finish up
            reduce (EndBlock s) state

        EndVerbatimBlock s ->
            -- TODO: finish up
            reduce (EndVerbatimBlock s) state

        Problem s ->
            -- TODO: finish up
            state



-- HANDLERS


handleBlankLine indent state =
    -- TODO: finish up
    if BP.level indent == BP.level state.indent then
        case List.head state.stack of
            Nothing ->
                state

            Just blockM ->
                if List.member (BP.typeOfBlock blockM.content) [ P, V ] then
                    { state | stack = BP.appendLineAtTop "" state.stack, indent = indent }

                else
                    state

    else
        state


handleOrdinaryLine indent line state =
    if BP.level indent >= BP.blockLevelOfStackTop state.stack then
        let
            _ =
                debug3 "BRANCH 3" "Shift or append line at top"
        in
        case List.head state.stack of
            Nothing ->
                BP.shift (Paragraph [ String.dropLeft indent line ]) { state | indent = indent }

            Just blockM ->
                if BP.typeOfBlock blockM.content == P then
                    { state | stack = BP.appendLineAtTop (String.dropLeft indent line) state.stack, indent = indent }

                else
                    BP.shift (Paragraph [ String.dropLeft indent line ]) { state | indent = indent }

    else if BP.level indent < BP.blockLevelOfStackTop state.stack then
        -- else if BP.level indent > BP.level state.indent + 1 then
        let
            _ =
                debug3 "BRANCH 1" "Shift, reduceStack"
        in
        BP.shift (Paragraph [ line ]) (BP.reduceStack { state | indent = indent })

    else
        -- else if BP.level indent > BP.blockLevelOfStackTop state.stack then
        let
            _ =
                debug3 "BRANCH 2" "INACCESSIBLE ??"
        in
        state |> reduce OrdinaryLine |> BP.shift (Paragraph [ String.dropLeft indent line ]) |> (\st -> { st | indent = indent })



-- STACK
-- REDUCE


reduce : Line.LineType -> State -> State
reduce lineType state =
    state
