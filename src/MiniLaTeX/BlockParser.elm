module MiniLaTeX.BlockParser exposing (run, runFromString)

import Common.BlockParser as BP exposing (State, Step(..), loop)
import Common.Debug exposing (debug1, debug2, debug3)
import Common.Line as Line exposing (LineType(..))
import Common.Syntax exposing (BasicBlock(..), Block, BlockType(..))
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
                { state | indent = indent } |> BP.reduceStack |> BP.shift (BBBlock s [])

            else
                { state | indent = indent } |> BP.shift (BBBlock s [])

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
                    { state | indent = indent } |> BP.reduceStack |> BP.shift (BBVerbatimBlock s [])

            else
                { state | indent = indent } |> BP.shift (BBVerbatimBlock s [])

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

                    errorMessage : Block
                    errorMessage =
                        { content = BBParagraph [ "Error: I was expecting an end-block labeled  " ++ s2 ++ ", but found " ++ s ], meta = Just <| Common.Syntax.dummyMeta 0 0 }
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
        case List.head state.stack of
            Nothing ->
                BP.shift (BBParagraph [ String.dropLeft indent line ]) { state | indent = indent }

            Just blockM ->
                if BP.typeOfBlock blockM.content == P then
                    { state | stack = BP.appendLineAtTop (String.dropLeft indent line) state.stack, indent = indent }

                else
                    BP.shift (BBParagraph [ String.dropLeft indent line ]) { state | indent = indent }

    else
        BP.shift (BBParagraph [ line ]) (BP.reduceStack { state | indent = indent })



-- REDUCE


reduce : Line.LineType -> State -> State
reduce lineType state =
    state



-- DEBUG
