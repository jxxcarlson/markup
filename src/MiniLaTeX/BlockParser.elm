module MiniLaTeX.BlockParser exposing (Block(..), run, runFromString)

import Console
import Line exposing (LineType(..))
import List.Extra
import MiniLaTeX.Line as Line
import Utility


type Block
    = Paragraph (List String)
    | VerbatimBlock String (List String)
    | Block String (List Block)


type BlockType
    = P
    | V
    | B


type alias BlockM =
    { content : Block, meta : Maybe Meta }


type alias Meta =
    { start : Int
    , end : Int
    , indent : Int
    , id : String
    }


dummyMeta : Int -> Int -> Meta
dummyMeta start indent =
    { start = start, end = start, indent = indent, id = "76" }


type alias State =
    { input : List String
    , output : List BlockM
    , indent : Int
    , lineNumber : Int
    , generation : Int
    , blockCount : Int
    , counter : Int
    , stack : List BlockM
    }


runFromString : Int -> String -> State
runFromString k str =
    run k (String.lines str)


run : Int -> List String -> State
run generation input =
    loop (initialState generation input) nextState


initialState : Int -> List String -> State
initialState generation input =
    { input = input
    , output = []
    , indent = 0
    , lineNumber = 0
    , generation = generation
    , blockCount = 0
    , counter = 0
    , stack = []
    }


nextState : State -> Step State State
nextState state =
    let
        _ =
            debug3 "-------------------------------------" "-"

        _ =
            debug3 "INPUT" ( state.counter, state.input )

        _ =
            debug1 "OUTPUT" ( state.counter, List.map .content state.output )

        _ =
            debug2 "STACK" ( state.counter, List.map .content state.stack, blockLevelOfStackTop state.stack )
    in
    case List.head state.input of
        Nothing ->
            let
                newState =
                    reduceStack { state | counter = state.counter + 1 }

                -- |> reverseStack
                _ =
                    debug1 "STACK" ( newState.counter, List.map .content newState.stack, blockLevelOfStackTop newState.stack )

                _ =
                    debug1 "Reduce stack" (newState.output |> List.map .content)

                finalState =
                    { newState | output = newState.stack ++ newState.output |> List.reverse }

                _ =
                    finalState |> .input |> debug1 "INPUT"

                _ =
                    finalState |> .output |> List.map .content |> debug1 "OUTPUT"
            in
            Done finalState

        Just line ->
            Loop (nextStateAux line { state | counter = state.counter + 1, input = List.drop 1 state.input })


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
            if level indent <= blockLevelOfStackTop state.stack then
                { state | indent = indent } |> reduceStack |> shift (Block s [])

            else
                { state | indent = indent } |> shift (Block s [])

        BeginVerbatimBlock s ->
            if level indent <= blockLevelOfStackTop state.stack then
                let
                    _ =
                        debug1 "(BeginVerbatimBlock)" s

                    yada =
                        Utility.takeUntil (\a -> blockLabelM a == s && blockLevel a == level indent) state.stack

                    _ =
                        yada.prefix |> blockLabelAtBottomOfStack |> debug2 "yada.prefix, bottom label"
                in
                if blockLabelAtBottomOfStack yada.prefix == s then
                    { state | indent = indent } |> reduceStack

                else
                    { state | indent = indent } |> reduceStack |> shift (VerbatimBlock s [])

            else
                { state | indent = indent } |> shift (VerbatimBlock s [])

        OrdinaryLine ->
            state |> handleOrdinaryLine indent line

        BlankLine ->
            handleBlankLine indent state

        EndBlock s ->
            -- TODO: finish up
            let
                { prefix, rest } =
                    Utility.takeUntil (\a -> blockLabelM a == s && blockLevel a == level indent) state.stack

                data =
                    reduceStack_ { stack = prefix, output = [] } |> debug1 "data (1), Endblock"
            in
            if s == blockLabelAtBottomOfStack prefix then
                { state | stack = data.stack ++ rest, output = data.output ++ state.output }

            else
                let
                    s2 =
                        blockLabelAtBottomOfStack prefix

                    errorMessage : BlockM
                    errorMessage =
                        { content = Paragraph [ "Error: I was expecting an end-block labeled  " ++ s2 ++ ", but found " ++ s ], meta = Just <| dummyMeta 0 0 }
                in
                { state | stack = data.stack ++ rest, output = errorMessage :: data.output ++ state.output }

        EndVerbatimBlock s ->
            -- TODO: finish up
            reduce (EndVerbatimBlock s) state

        Problem s ->
            -- TODO: finish up
            state


reduceN : List BlockM -> List BlockM
reduceN =
    identity



-- HANDLERS


handleBlankLine indent state =
    -- TODO: finish up
    if level indent == level state.indent then
        case List.head state.stack of
            Nothing ->
                state

            Just blockM ->
                if List.member (typeOfBlock blockM.content) [ P, V ] then
                    { state | stack = appendLineAtTop "" state.stack, indent = indent }

                else
                    state

    else
        reduceStack state


handleOrdinaryLine indent line state =
    if level indent >= blockLevelOfStackTop state.stack then
        let
            _ =
                debug3 "handleOrdinaryLine, BRANCH 3" "Shift or append line at top"
        in
        case List.head state.stack of
            Nothing ->
                shift (Paragraph [ String.dropLeft indent line ]) { state | indent = indent }

            Just blockM ->
                if typeOfBlock blockM.content == P then
                    { state | stack = appendLineAtTop (String.dropLeft indent line) state.stack, indent = indent }

                else
                    shift (Paragraph [ String.dropLeft indent line ]) { state | indent = indent }

    else if level indent < blockLevelOfStackTop state.stack then
        -- else if level indent > level state.indent + 1 then
        let
            _ =
                debug3 "handleOrdinaryLine, BRANCH 1" "Shift, reduceStack"
        in
        shift (Paragraph [ line ]) (reduceStack { state | indent = indent })

    else
        -- else if level indent > blockLevelOfStackTop state.stack then
        let
            _ =
                debug3 "handleOrdinaryLine, BRANCH 2" "INACCESSIBLE ??"
        in
        state |> reduce OrdinaryLine |> shift (Paragraph [ String.dropLeft indent line ]) |> (\st -> { st | indent = indent })



-- STACK


reverseStack : State -> State
reverseStack state =
    { state | stack = List.reverse state.stack }


reduceStack_ : { stack : List BlockM, output : List BlockM } -> { stack : List BlockM, output : List BlockM }
reduceStack_ state =
    case List.Extra.uncons state.stack of
        Nothing ->
            state

        Just ( blockM1, stack2 ) ->
            case List.Extra.uncons stack2 of
                Nothing ->
                    { stack = stack2, output = [ reverseContentsM blockM1 ] }

                Just ( blockM2, stack3 ) ->
                    if blockLevel blockM1 <= blockLevel blockM2 then
                        let
                            _ =
                                debug1 "blockLevel blockM1 <= blockLevel blockM2" True
                        in
                        state

                    else
                        case blockM2.content of
                            Block name blocks ->
                                let
                                    _ =
                                        debug1 "reduceStack Block" True

                                    newBlock : BlockM
                                    newBlock =
                                        { content = Block name (reverseContents blockM1.content :: blocks), meta = blockM2.meta }
                                in
                                reduceStack_ { state | stack = stack3, output = newBlock :: state.output }

                            VerbatimBlock name blocks ->
                                case blockM1.content of
                                    Paragraph strings ->
                                        let
                                            newBlock : BlockM
                                            newBlock =
                                                { content = VerbatimBlock name (List.reverse <| strings ++ blocks), meta = blockM2.meta }
                                        in
                                        reduceStack_ { state | stack = stack3, output = newBlock :: state.output }

                                    _ ->
                                        state

                            _ ->
                                state


reduceStack : State -> State
reduceStack state =
    let
        _ =
            debug1 "reduceStack" True
    in
    case List.Extra.uncons state.stack of
        Nothing ->
            state

        Just ( blockM1, stack2 ) ->
            case List.Extra.uncons stack2 of
                Nothing ->
                    let
                        _ =
                            debug1 "reduceStack, len = " 1
                    in
                    { state | output = reverseContentsM blockM1 :: state.output, stack = stack2 }

                Just ( blockM2, stack3 ) ->
                    let
                        _ =
                            debug1 "reduceStack, (level(1), level(2)" ( blockLevel blockM1, blockLevel blockM2 )
                    in
                    if blockLevel blockM1 <= blockLevel blockM2 then
                        let
                            _ =
                                debug1 "blockLevel blockM1 <= blockLevel blockM2" True
                        in
                        state

                    else
                        case blockM2.content of
                            Block name blocks ->
                                let
                                    _ =
                                        debug1 "reduceStack Block" True

                                    newBlock : BlockM
                                    newBlock =
                                        { content = Block name (reverseContents blockM1.content :: blocks), meta = blockM2.meta }
                                in
                                reduceStack { state | stack = stack3, output = newBlock :: state.output }

                            VerbatimBlock name blocks ->
                                let
                                    _ =
                                        debug1 "reduceStack VerbatimBlock" True
                                in
                                case blockM1.content of
                                    Paragraph strings ->
                                        let
                                            newBlock : BlockM
                                            newBlock =
                                                { content = VerbatimBlock name (List.reverse <| strings ++ blocks), meta = blockM2.meta }
                                        in
                                        reduceStack { state | stack = stack3, output = newBlock :: state.output }

                                    _ ->
                                        state

                            _ ->
                                state


appendLineAtTop line stack =
    -- TODO: check this
    case List.head stack of
        Nothing ->
            stack

        Just block ->
            case block.content of
                Paragraph strings ->
                    { content = Paragraph (line :: strings), meta = block.meta } :: List.drop 1 stack

                _ ->
                    stack



-- SHIFT


shift : Block -> State -> State
shift block state =
    let
        newBlock =
            { content = block
            , meta =
                Just
                    { start = state.lineNumber
                    , end = state.lineNumber
                    , indent = state.indent
                    , id = String.fromInt state.generation ++ "." ++ String.fromInt state.blockCount
                    }
            }
    in
    { state
        | stack = newBlock :: state.stack
        , lineNumber = state.lineNumber + 1
        , blockCount = state.blockCount + 1
    }



-- REDUCE


reduce : Line.LineType -> State -> State
reduce lineType state =
    state



-- LOOP


type Step state a
    = Loop state
    | Done a


loop : State -> (State -> Step State State) -> State
loop s nextState_ =
    case nextState_ s of
        Loop s_ ->
            loop s_ nextState_

        Done b ->
            b



-- HELPERS


quantumOfIndentation =
    3


level : Int -> Int
level indentation =
    indentation // quantumOfIndentation


blockLevel : BlockM -> Int
blockLevel blockM =
    Maybe.map .indent blockM.meta |> Maybe.withDefault 0 |> level


blockLevelOfStackTop : List BlockM -> Int
blockLevelOfStackTop stack =
    case List.head stack of
        Nothing ->
            0

        Just blockM ->
            blockLevel blockM


typeOfBlock : Block -> BlockType
typeOfBlock b =
    case b of
        Paragraph _ ->
            P

        VerbatimBlock _ _ ->
            V

        Block _ _ ->
            B


blockLabel : Block -> String
blockLabel block =
    case block of
        Paragraph _ ->
            "(no label)"

        Block s _ ->
            s

        VerbatimBlock s _ ->
            s


blockLabelM : BlockM -> String
blockLabelM block =
    blockLabel block.content


blockLabelAtTopOfStack : List BlockM -> String
blockLabelAtTopOfStack stack =
    case List.head stack of
        Nothing ->
            "(no label)"

        Just block ->
            blockLabelM block


blockLabelAtBottomOfStack : List BlockM -> String
blockLabelAtBottomOfStack stack =
    case List.head (List.reverse stack) of
        Nothing ->
            "(no label)"

        Just block ->
            blockLabelM block


sameKindOfBlock : Block -> Block -> Bool
sameKindOfBlock a b =
    typeOfBlock a == typeOfBlock b


blockIsLikeTopOfStack : Block -> List BlockM -> Bool
blockIsLikeTopOfStack block blocks =
    case List.head blocks of
        Nothing ->
            False

        Just blockM ->
            sameKindOfBlock block blockM.content


reverseContents : Block -> Block
reverseContents block =
    case block of
        Paragraph strings ->
            Paragraph (List.reverse strings)

        VerbatimBlock name strings ->
            VerbatimBlock name (List.reverse strings)

        Block name strings ->
            Block name (List.reverse strings)


reverseContentsM : BlockM -> BlockM
reverseContentsM { content, meta } =
    { content = reverseContents content, meta = meta }



-- DEBUG


debugOn =
    True


debug1 str =
    if debugOn then
        Debug.log (Console.magenta str)

    else
        identity


debug2 str =
    if debugOn then
        Debug.log (Console.cyan str)

    else
        identity


debug3 str =
    if debugOn then
        Debug.log (Console.yellow str)

    else
        identity
