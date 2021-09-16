module MiniLaTeX.BlockParser exposing (run, runFromString)

import Common.BlockParser as BP exposing (State, Step(..), loop)
import Common.Debug exposing (debug1, debug2, debug3)
import Common.Line as Line exposing (LineType(..))
import Common.Syntax exposing (Block(..), BlockM, BlockType(..))
import MiniLaTeX.Line as Line
import Utility


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
                let
                    _ =
                        debug1 "(BeginVerbatimBlock)" s

                    yada =
                        Utility.takeUntil (\a -> BP.blockLabelM a == s && BP.blockLevel a == BP.level indent) state.stack

                    _ =
                        yada.prefix |> BP.blockLabelAtBottomOfStack |> debug2 "yada.prefix, bottom label"
                in
                if BP.blockLabelAtBottomOfStack yada.prefix == s then
                    { state | indent = indent } |> BP.reduceStack

                else
                    { state | indent = indent } |> BP.reduceStack |> BP.shift (VerbatimBlock s [])

            else
                { state | indent = indent } |> BP.shift (VerbatimBlock s [])

        OrdinaryLine ->
            state |> handleOrdinaryLine indent line

        BlankLine ->
            handleBlankLine indent state

        EndBlock s ->
            -- TODO: finish up
            let
                { prefix, rest } =
                    Utility.takeUntil (\a -> BP.blockLabelM a == s && BP.blockLevel a == BP.level indent) state.stack

                data =
                    BP.reduceStack_ { stack = prefix, output = [] } |> debug1 "data (1), Endblock"
            in
            if s == BP.blockLabelAtBottomOfStack prefix then
                { state | stack = data.stack ++ rest, output = data.output ++ state.output }

            else
                let
                    s2 =
                        BP.blockLabelAtBottomOfStack prefix

                    errorMessage : BlockM
                    errorMessage =
                        { content = Paragraph [ "Error: I was expecting an end-block labeled  " ++ s2 ++ ", but found " ++ s ], meta = Just <| Common.Syntax.dummyMeta 0 0 }
                in
                { state | stack = data.stack ++ rest, output = errorMessage :: data.output ++ state.output }

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
        BP.reduceStack state


handleOrdinaryLine indent line state =
    if BP.level indent >= BP.blockLevelOfStackTop state.stack then
        let
            _ =
                debug3 "handleOrdinaryLine, BRANCH 3" "Shift or append line at top"
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
        -- else if level indent > level state.indent + 1 then
        let
            _ =
                debug3 "handleOrdinaryLine, BRANCH 1" "Shift, reduceStack"
        in
        BP.shift (Paragraph [ line ]) (BP.reduceStack { state | indent = indent })

    else
        -- else if level indent > blockLevelOfStackTop state.stack then
        let
            _ =
                debug3 "handleOrdinaryLine, BRANCH 2" "INACCESSIBLE ??"
        in
        state |> reduce OrdinaryLine |> BP.shift (Paragraph [ String.dropLeft indent line ]) |> (\st -> { st | indent = indent })



-- REDUCE


reduce : Line.LineType -> State -> State
reduce lineType state =
    state



-- DEBUG
