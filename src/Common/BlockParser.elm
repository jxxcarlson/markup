module Common.BlockParser exposing (classify, parse, runParser)

import Common.BlockParserTools as BP exposing (State, level)
import Common.Debug exposing (debug1, debug2, debug3)
import Common.Library.ParserTools as ParserTools
import Common.Line as Line exposing (LineType(..))
import Common.Syntax as Syntax exposing (Block(..), BlockType(..), Language(..))
import L1.Line
import Markdown.Line
import MiniLaTeX.Line
import MiniLaTeX.MathMacro
import Utility


parse : Language -> Int -> List String -> List Block
parse language generation lines =
    lines |> runParser language generation |> .output


runParser : Language -> Int -> List String -> State
runParser language generation input =
    BP.loop (BP.initialState generation input) (BP.nextStep (nextStateAux language))


type alias LineData =
    { indent : Int, lineType : Line.LineType, content : String }


classify : Language -> Bool -> String -> LineData
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
        lineType =
            classify language state.inVerbatimBlock line |> debug2 "lineType (nextStateAux)"

        inVerbatimBlock =
            isInVerbatimBlock lineType state

        adjustedLineType =
            adjustLineType lineType inVerbatimBlock

        indent =
            lineType.indent
    in
    nextStateAux2 indent line adjustedLineType lineType { state | inVerbatimBlock = inVerbatimBlock }


nextStateAux2 : Int -> String -> LineType -> LineData -> State -> State
nextStateAux2 indent line adjustedLineType lineData state =
    case adjustedLineType of
        BeginBlock Line.AcceptFirstLine s ->
            handleBeginBlock1 lineData indent state s

        BeginBlock Line.RejectFirstLine s ->
            handleBeginBlock2 indent state s

        BeginVerbatimBlock s ->
            handleBeginVerbatimBlock indent state s

        OrdinaryLine ->
            state |> handleOrdinaryLine indent line |> (\s -> { s | lineNumber = s.lineNumber + 1 |> debug2 "OrdinaryLine, line" })

        VerbatimLine ->
            state |> handleVerbatimLine indent line

        BlankLine ->
            handleBlankLine indent state

        EndBlock s ->
            handleEndBlock indent state s

        EndVerbatimBlock s ->
            -- TODO: finish up
            reduce (EndVerbatimBlock s) state

        Problem s ->
            -- TODO: finish up
            state



-- HELPERS AND HANDLERS


adjustLineType lineType inVerbatimBlock =
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


isInVerbatimBlock lineType state =
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


handleEndBlock indent state s =
    -- TODO: finish up
    let
        { prefix, rest } =
            Utility.takeUntil (\a -> BP.blockLabel a == s && BP.blockLevel a == BP.level indent) state.stack

        data =
            BP.reduceStack_ { stack = prefix, output = [] } |> debug1 "data (1), yada0"

        stringListToString list =
            List.map String.trimLeft list |> String.join "\n"

        verbatimBlockData =
            case List.head data.output of
                Just (VerbatimBlock "mathmacro" strList _) ->
                    ( "mathmacro", stringListToString strList )

                _ ->
                    ( "nothing", "" )

        oldAccumulator =
            state.accumulator

        newAccumulator =
            case verbatimBlockData of
                ( "mathmacro", mathMacroData ) ->
                    { oldAccumulator | macroDict = MiniLaTeX.MathMacro.makeMacroDict mathMacroData }

                _ ->
                    oldAccumulator
    in
    if s == BP.blockLabelAtBottomOfStack prefix then
        { state
            | stack = data.stack ++ rest
            , output = data.output ++ state.output
            , accumulator = newAccumulator
        }

    else
        let
            s2 =
                BP.blockLabelAtBottomOfStack prefix

            errorMessage : Block
            errorMessage =
                BlockError <| "Error: I was expecting an end-block labeled  " ++ s2 ++ ", but found " ++ s
        in
        { state | stack = data.stack ++ rest, output = errorMessage :: data.output ++ state.output }


handleBeginVerbatimBlock indent state s =
    if BP.level indent <= BP.blockLevelOfStackTop state.stack then
        let
            yada =
                Utility.takeUntil (\a -> BP.blockLabel a == s && BP.blockLevel a == BP.level indent) state.stack
        in
        if BP.blockLabelAtBottomOfStack yada.prefix == s then
            { state | indent = indent, verbatimBlockInitialIndent = indent + 3 } |> BP.reduceStack

        else
            { state | indent = indent, verbatimBlockInitialIndent = indent + 3 } |> BP.reduceStack |> BP.shift (VerbatimBlock s [] (Syntax.dummyMeta 0 0))

    else
        { state | indent = indent, verbatimBlockInitialIndent = indent + 3 } |> BP.shift (VerbatimBlock s [] (Syntax.dummyMeta 0 0))


handleBeginBlock1 lineData indent state s =
    let
        innerBlockList =
            if lineData.content == "" then
                []

            else
                [ Paragraph [ lineData.content ] (Syntax.dummyMeta 0 0) ]
    in
    if BP.level indent <= BP.blockLevelOfStackTop state.stack then
        { state | indent = indent } |> BP.reduceStack |> BP.shift (Block s innerBlockList (Syntax.dummyMeta 0 0))

    else
        { state | indent = indent } |> BP.shift (Block s innerBlockList (Syntax.dummyMeta 0 0))


handleBeginBlock2 indent state s =
    if BP.level indent <= BP.blockLevelOfStackTop state.stack then
        { state | indent = indent } |> BP.reduceStack |> BP.shift (Block s [] (Syntax.dummyMeta 0 0))

    else
        { state | indent = indent } |> BP.shift (Block s [] (Syntax.dummyMeta 0 0))


handleBlankLine indent state =
    -- TODO: finish up
    if BP.level indent == BP.level state.indent then
        case List.head state.stack of
            Nothing ->
                state

            Just block ->
                if List.member (BP.typeOfBlock block) [ V ] then
                    { state | stack = BP.appendLineAtTop state.lineNumber "" state.stack, indent = indent }

                else
                    -- { state | stack = [], output = (Syntax.Paragraph [] (Syntax.dummyMeta 0 0) :: List.reverse state.stack) ++ state.output }
                    { state | stack = [], output = (Syntax.Paragraph [] (Syntax.dummyMeta 0 0) :: List.map BP.reverseContents (List.reverse state.stack)) ++ state.output }

    else
        BP.reduceStack state


handleOrdinaryLine indent line state =
    if BP.level indent >= BP.blockLevelOfStackTop state.stack then
        case List.head state.stack of
            Nothing ->
                BP.shift (Paragraph [ String.dropLeft indent (line ++ "\n") ] (Syntax.dummyMeta 0 0)) { state | indent = indent }

            Just block ->
                if List.member (BP.typeOfBlock block) [ P, B ] then
                    { state | stack = BP.appendLineAtTop state.lineNumber (String.dropLeft indent (line ++ "\n")) state.stack, indent = indent }

                else
                    BP.shift (Paragraph [ String.dropLeft indent (line ++ "\n") ] (Syntax.dummyMeta 0 0)) { state | indent = indent }

    else
        BP.shift (Paragraph [ line ++ "\n" ] (Syntax.dummyMeta 0 0)) (BP.reduceStack { state | indent = indent })


handleVerbatimLine indent line state =
    if BP.level indent >= BP.blockLevelOfStackTop state.stack then
        case List.head state.stack of
            Nothing ->
                BP.shift (Paragraph [ String.dropLeft 0 line ] (Syntax.dummyMeta 0 0)) { state | indent = indent }

            Just block ->
                if BP.typeOfBlock block == P then
                    { state | stack = BP.appendLineAtTop state.lineNumber (String.dropLeft 0 line) state.stack, indent = indent }

                else
                    BP.shift (Paragraph [ String.dropLeft 0 line ] (Syntax.dummyMeta 0 0)) { state | indent = indent }

    else
        BP.shift (Paragraph [ line ] (Syntax.dummyMeta 0 0)) (BP.reduceStack { state | indent = indent })



-- REDUCE


reduce : Line.LineType -> State -> State
reduce lineType state =
    state



-- DEBUG
