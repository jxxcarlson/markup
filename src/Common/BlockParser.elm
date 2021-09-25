module Common.BlockParser exposing (classify, parse, runParser)

import Common.BlockParserTools as BP exposing (State, Step(..), level, loop, reduceStack)
import Common.Debug exposing (debug1, debug2, debug3)
import Common.Library.ParserTools as ParserTools
import Common.Line as Line exposing (LineType(..))
import Common.Syntax as Syntax exposing (Block(..), BlockType(..), Language(..))
import L1.Line
import Markdown.Line
import MiniLaTeX.Line
import Utility


parse : Language -> Int -> List String -> List Block
parse language generation lines =
    lines |> runParser language generation |> .output


runParser : Language -> Int -> List String -> State
runParser language generation input =
    BP.loop (BP.initialState generation input) (BP.nextStep (nextStateAux language))


classify : Language -> Bool -> String -> { indent : Int, lineType : Line.LineType, content : String }
classify language inVerbatimBlock str =
    let
        lineType =
            getLineTypeParser language

        leadingSpaces =
            Line.countLeadingSpaces str

        nibble str_ =
            String.dropLeft (String.length (ParserTools.nibble str_) + 1) str_

        provisionalLineType =
            lineType (String.dropLeft leadingSpaces str)

        lineType_ =
            if inVerbatimBlock && provisionalLineType == Line.BlankLine then
                Line.VerbatimLine

            else
                provisionalLineType
    in
    { indent = leadingSpaces, lineType = lineType_, content = nibble str }


classify1 : (String -> Line.LineType) -> Bool -> String -> { indent : Int, lineType : Line.LineType, content : String }
classify1 lineType inVerbatimBlock str =
    let
        leadingSpaces =
            Line.countLeadingSpaces str

        nibble str_ =
            String.dropLeft (String.length (ParserTools.nibble str_) + 1) str_

        provisionalLineType =
            lineType (String.dropLeft leadingSpaces str)

        lineType_ =
            if inVerbatimBlock && provisionalLineType == Line.BlankLine then
                Line.VerbatimLine

            else
                provisionalLineType
    in
    { indent = leadingSpaces, lineType = lineType_, content = nibble str }


getLineTypeParser : Language -> String -> Line.LineType
getLineTypeParser language =
    case language of
        L1 ->
            L1.Line.lineType

        Markdown ->
            Markdown.Line.lineType

        MiniLaTeX ->
            MiniLaTeX.Line.lineType


nextStateAux : Language -> String -> State -> State
nextStateAux language line state =
    let
        _ =
            debug2 "(state.indent, state.verbatimBlockInitialIndent)" ( state.verbatimBlockInitialIndent, state.indent )

        lineType =
            classify language state.inVerbatimBlock line |> debug2 "lineType (nextStateAux)"

        inVerbatimBlock =
            (case lineType.lineType of
                BeginVerbatimBlock _ ->
                    True

                _ ->
                    -- TODO: check this out.  Is it OK?? Previously: < level state.indent
                    if level lineType.indent < level state.verbatimBlockInitialIndent then
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
    let
        _ =
            debug1 "newLineType" newLineType
    in
    case newLineType of
        BeginBlock Line.AcceptFirstLine s ->
            let
                innerBlockList =
                    if lineType.content == "" then
                        []

                    else
                        [ Paragraph [ lineType.content ] (Syntax.dummyMeta 0 0) ]
            in
            if BP.level indent <= BP.blockLevelOfStackTop state.stack then
                { state | indent = indent } |> BP.reduceStack |> BP.shift (Block s innerBlockList (Syntax.dummyMeta 0 0))

            else
                { state | indent = indent } |> BP.shift (Block s innerBlockList (Syntax.dummyMeta 0 0))

        BeginBlock Line.RejectFirstLine s ->
            if BP.level indent <= BP.blockLevelOfStackTop state.stack then
                { state | indent = indent } |> BP.reduceStack |> BP.shift (Block s [] (Syntax.dummyMeta 0 0))

            else
                { state | indent = indent } |> BP.shift (Block s [] (Syntax.dummyMeta 0 0))

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
                    { state | indent = indent, verbatimBlockInitialIndent = indent + 3 } |> BP.reduceStack

                else
                    { state | indent = indent, verbatimBlockInitialIndent = indent + 3 } |> BP.reduceStack |> BP.shift (VerbatimBlock s [] (Syntax.dummyMeta 0 0))

            else
                { state | indent = indent, verbatimBlockInitialIndent = indent + 3 } |> BP.shift (VerbatimBlock s [] (Syntax.dummyMeta 0 0))

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
                    -- { state | stack = [], output = (Syntax.Paragraph [] (Syntax.dummyMeta 0 0) :: List.reverse state.stack) ++ state.output }
                    { state | stack = [], output = (Syntax.Paragraph [] (Syntax.dummyMeta 0 0) :: List.map BP.reverseContents (List.reverse state.stack)) ++ state.output }

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
            debug1 "!!! handleOrdinaryLine, (indent, line, inVerbatimBlock)" ( indent, line, state.inVerbatimBlock )
    in
    if BP.level indent >= BP.blockLevelOfStackTop state.stack then
        case List.head state.stack of
            Nothing ->
                let
                    _ =
                        debug1 "handleOrdinaryLine, (indent, line)" 1
                in
                BP.shift (Paragraph [ String.dropLeft indent (line ++ "\n") ] (Syntax.dummyMeta 0 0)) { state | indent = indent }

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
                    { state | stack = BP.appendLineAtTop (String.dropLeft indent (line ++ "\n")) state.stack, indent = indent }

                else
                    let
                        _ =
                            debug1 "handleOrdinaryLine, (indent, line)" 2.2
                    in
                    BP.shift (Paragraph [ String.dropLeft indent (line ++ "\n") ] (Syntax.dummyMeta 0 0)) { state | indent = indent }

    else
        let
            _ =
                debug1 "handleOrdinaryLine, (indent, line)" 3
        in
        BP.shift (Paragraph [ line ++ "\n" ] (Syntax.dummyMeta 0 0)) (BP.reduceStack { state | indent = indent })


handleVerbatimLine indent line state =
    let
        _ =
            debug1 "handleVerbatimLine" line
    in
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
