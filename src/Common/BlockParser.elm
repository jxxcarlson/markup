module Common.BlockParser exposing (parse, run)

import Common.BasicSyntax as Basic exposing (BasicBlock(..))
import Common.BlockParserTools as BP exposing (State, Step(..), level, loop)
import Common.Debug exposing (debug1, debug2, debug3)
import Common.Line as Line exposing (LineType(..))
import Common.Syntax as Syntax exposing (Block(..), BlockType(..))
import Markdown.Line as Line
import Utility


parse : Int -> List String -> List Block
parse generation lines =
    lines |> run generation |> .output


run : Int -> List String -> State
run generation input =
    BP.loop (BP.initialState generation input) (BP.nextStep nextStateAux)


nextStateAux : String -> State -> State
nextStateAux line state =
    let
        lineType =
            Line.classify line |> debug2 "lineType"

        inVerbatimBlock =
            (case lineType.lineType of
                BeginVerbatimBlock _ ->
                    True

                _ ->
                    if level lineType.indent < level state.indent then
                        False

                    else
                        state.inVerbatimBlock
            )
                |> debug3 "inVerbatimBlock"

        newLineType =
            case lineType.lineType of
                BeginVerbatimBlock _ ->
                    lineType.lineType

                BlankLine ->
                    if inVerbatimBlock then
                        BlankLine

                    else
                        BlankLine

                _ ->
                    if inVerbatimBlock then
                        VerbatimLine

                    else
                        lineType.lineType

        indent =
            lineType.indent
    in
    nextStateAux2 indent line newLineType lineType { state | inVerbatimBlock = inVerbatimBlock }


nextStateAux2 indent line newLineType lineType state =
    case newLineType of
        BeginBlock s ->
            let
                innerBlock =
                    Paragraph [ lineType.content ] (Syntax.dummyMeta 0 0)
            in
            if BP.level indent <= BP.blockLevelOfStackTop state.stack then
                { state | indent = indent } |> BP.reduceStack |> BP.shift (Block s [ innerBlock ] (Syntax.dummyMeta 0 0))

            else
                { state | indent = indent } |> BP.shift (Block s [ innerBlock ] (Syntax.dummyMeta 0 0))

        BeginVerbatimBlock s ->
            if BP.level indent <= BP.blockLevelOfStackTop state.stack then
                let
                    _ =
                        debug1 "(BeginVerbatimBlock)" s

                    yada =
                        Utility.takeUntil (\a -> BP.blockLabel a == s && BP.blockLevel a == BP.level indent) state.stack

                    _ =
                        yada.prefix |> BP.blockLabelAtBottomOfStack |> debug2 "yada.prefix, bottom label"
                in
                if BP.blockLabelAtBottomOfStack yada.prefix == s then
                    { state | indent = indent } |> BP.reduceStack

                else
                    { state | indent = indent } |> BP.reduceStack |> BP.shift (VerbatimBlock s [] (Syntax.dummyMeta 0 0))

            else
                { state | indent = indent } |> BP.shift (VerbatimBlock s [] (Syntax.dummyMeta 0 0))

        OrdinaryLine ->
            state |> handleOrdinaryLine indent line

        VerbatimLine ->
            state |> handleVerbatimLine indent line

        BlankLine ->
            handleBlankLine indent state

        EndBlock s ->
            -- TODO: finish up
            let
                { prefix, rest } =
                    Utility.takeUntil (\a -> BP.blockLabel a == s && BP.blockLevel a == BP.level indent) state.stack

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
                        Error <| "Error: I was expecting an end-block labeled  " ++ s2 ++ ", but found " ++ s
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

            Just block ->
                if List.member (BP.typeOfBlock block) [ V ] then
                    { state | stack = BP.appendLineAtTop "" state.stack, indent = indent }

                else
                    { state | stack = [], output = Syntax.Paragraph [] (Syntax.dummyMeta 0 0) :: List.reverse state.stack ++ state.output }

    else
        BP.reduceStack state



--handleVerbatimBlankLine indent state =
--    -- TODO: finish up
--    if BP.level indent >= BP.blockLevelOfStackTop state.stack then
--        case List.head state.stack of
--            Nothing ->
--                BP.shift (Paragraph [ String.dropLeft 0 line ] (Syntax.dummyMeta 0 0)) { state | indent = indent }
--
--            Just block ->
--                if BP.typeOfBlock block == P then
--                    { state | stack = BP.appendLineAtTop (String.dropLeft 0 line) state.stack, indent = indent }
--
--                else
--                    BP.shift (Paragraph [ String.dropLeft 0 line ] (Syntax.dummyMeta 0 0)) { state | indent = indent }
--
--    else
--        BP.shift (Paragraph [ line ] (Syntax.dummyMeta 0 0)) (BP.reduceStack { state | indent = indent })


handleOrdinaryLine indent line state =
    let
        _ =
            debug1 "handleOrdinaryLine, (indent, line)" ( indent, line )
    in
    if BP.level indent >= BP.blockLevelOfStackTop state.stack then
        case List.head state.stack of
            Nothing ->
                let
                    _ =
                        debug1 "handleOrdinaryLine, (indent, line)" 1
                in
                BP.shift (Paragraph [ String.dropLeft indent line ] (Syntax.dummyMeta 0 0)) { state | indent = indent }

            Just block ->
                let
                    _ =
                        debug1 "handleOrdinaryLine, typeOfBlock" (BP.typeOfBlock block)
                in
                if List.member (BP.typeOfBlock block) [ P, B ] then
                    let
                        _ =
                            debug1 "handleOrdinaryLine, (indent, line)" 2.1
                    in
                    { state | stack = BP.appendLineAtTop (String.dropLeft indent line) state.stack, indent = indent }

                else
                    let
                        _ =
                            debug1 "handleOrdinaryLine, (indent, line)" 2.2
                    in
                    BP.shift (Paragraph [ String.dropLeft indent line ] (Syntax.dummyMeta 0 0)) { state | indent = indent }

    else
        let
            _ =
                debug1 "handleOrdinaryLine, (indent, line)" 3
        in
        BP.shift (Paragraph [ line ] (Syntax.dummyMeta 0 0)) (BP.reduceStack { state | indent = indent })


handleVerbatimLine indent line state =
    if BP.level indent >= BP.blockLevelOfStackTop state.stack then
        case List.head state.stack of
            Nothing ->
                BP.shift (Paragraph [ String.dropLeft 0 line ] (Syntax.dummyMeta 0 0)) { state | indent = indent }

            Just block ->
                if BP.typeOfBlock block == P then
                    { state | stack = BP.appendLineAtTop (String.dropLeft 0 line) state.stack, indent = indent }

                else
                    BP.shift (Paragraph [ String.dropLeft 0 line ] (Syntax.dummyMeta 0 0)) { state | indent = indent }

    else
        BP.shift (Paragraph [ line ] (Syntax.dummyMeta 0 0)) (BP.reduceStack { state | indent = indent })



-- REDUCE


reduce : Line.LineType -> State -> State
reduce lineType state =
    state



-- DEBUG
