module Common.BlockParser exposing
    ( State
    , Step(..)
    , appendLineAtTop
    , blockLabel
    , blockLabelAtBottomOfStack
    , blockLevel
    , blockLevelOfStackTop
    , initialState
    , level
    , loop
    , nextState
    , reduceStack
    , reduceStack_
    , reverseStack
    , shift
    , typeOfBlock
    )

import Common.Debug exposing (debug1, debug2, debug3)
import Common.Syntax as Syntax exposing (Block(..), BlockType(..))
import List.Extra


type alias State =
    { input : List String
    , output : List Block
    , indent : Int
    , lineNumber : Int
    , generation : Int
    , blockCount : Int
    , counter : Int
    , stack : List Block
    }


nextState : (String -> State -> State) -> State -> Step State State
nextState nextStateAux state =
    let
        _ =
            debug2 "STACK" ( state.counter, List.map Syntax.simplify state.stack, blockLevelOfStackTop state.stack )
    in
    case List.head state.input of
        Nothing ->
            let
                newState =
                    reduceStack { state | counter = state.counter + 1 }

                -- |> reverseStack
                _ =
                    debug1 "STACK" ( newState.counter, List.map Syntax.simplify newState.stack, blockLevelOfStackTop newState.stack )

                _ =
                    debug1 "Reduce stack" (newState.output |> List.map Syntax.simplify)

                finalState =
                    { newState | output = newState.stack ++ newState.output |> List.reverse }

                _ =
                    finalState |> .output |> List.map Syntax.simplify |> debug1 "OUTPUT"
            in
            Done finalState

        Just line ->
            Loop (nextStateAux line { state | counter = state.counter + 1, input = List.drop 1 state.input })


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


reduceStack : State -> State
reduceStack state =
    let
        _ =
            debug1 "reduceStack" True
    in
    case List.Extra.uncons state.stack of
        Nothing ->
            state

        Just ( block1, stack1 ) ->
            case List.Extra.uncons stack1 of
                Nothing ->
                    let
                        _ =
                            debug1 "reduceStack, len = " 1
                    in
                    { state | output = reverseContents block1 :: state.output, stack = stack1 }

                Just ( block2, stack2 ) ->
                    let
                        _ =
                            debug1 "reduceStack, (level(1), level(2)" ( blockLevel block1, blockLevel block2 )
                    in
                    if blockLevel block1 <= blockLevel block2 then
                        let
                            _ =
                                debug1 "blockLevel blockM1 <= blockLevel blockM2" True
                        in
                        state

                    else
                        case block2 of
                            Block name blocks meta ->
                                let
                                    _ =
                                        debug1 "reduceStack Block" True

                                    newBlock : Block
                                    newBlock =
                                        Block name (reverseContents block1 :: blocks) meta
                                in
                                reduceStack { state | stack = stack2, output = newBlock :: state.output }

                            VerbatimBlock name blocks meta ->
                                let
                                    _ =
                                        debug1 "reduceStack VerbatimBlock" True
                                in
                                case block1 of
                                    Paragraph strings metaP ->
                                        let
                                            newBlock : Block
                                            newBlock =
                                                VerbatimBlock name (List.reverse <| strings ++ blocks) metaP
                                        in
                                        reduceStack { state | stack = stack2, output = newBlock :: state.output }

                                    _ ->
                                        state

                            _ ->
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


blockLevel : Block -> Int
blockLevel block =
    case block of
        Paragraph _ meta ->
            level meta.indent

        VerbatimBlock _ _ meta ->
            level meta.indent

        Block _ _ meta ->
            level meta.indent


blockLevelOfStackTop : List Block -> Int
blockLevelOfStackTop stack =
    case List.head stack of
        Nothing ->
            0

        Just blockM ->
            blockLevel blockM


typeOfBlock : Block -> BlockType
typeOfBlock b =
    case b of
        Paragraph _ _ ->
            P

        VerbatimBlock _ _ _ ->
            V

        Block _ _ _ ->
            B


blockLabel : Block -> String
blockLabel block =
    case block of
        Paragraph _ _ ->
            "(no label)"

        Block s _ _ ->
            s

        VerbatimBlock s _ _ ->
            s


blockLabelAtTopOfStack : List Block -> String
blockLabelAtTopOfStack stack =
    case List.head stack of
        Nothing ->
            "(no label)"

        Just block ->
            blockLabel block


blockLabelAtBottomOfStack : List Block -> String
blockLabelAtBottomOfStack stack =
    case List.head (List.reverse stack) of
        Nothing ->
            "(no label)"

        Just block ->
            blockLabel block


sameKindOfBlock : Block -> Block -> Bool
sameKindOfBlock a b =
    typeOfBlock a == typeOfBlock b


blockIsLikeTopOfStack : Block -> List Block -> Bool
blockIsLikeTopOfStack block blocks =
    case List.head blocks of
        Nothing ->
            False

        Just block2 ->
            sameKindOfBlock block block2


reverseContents : Block -> Block
reverseContents block =
    case block of
        Paragraph strings meta ->
            Paragraph (List.reverse strings) meta

        VerbatimBlock name strings meta ->
            VerbatimBlock name (List.reverse strings) meta

        Block name strings meta ->
            Block name (List.reverse strings) meta



-- STACK


reverseStack : State -> State
reverseStack state =
    { state | stack = List.reverse state.stack }


reduceStack_ : { stack : List Block, output : List Block } -> { stack : List Block, output : List Block }
reduceStack_ state =
    case List.Extra.uncons state.stack of
        Nothing ->
            state

        Just ( block1, stack1 ) ->
            case List.Extra.uncons stack1 of
                Nothing ->
                    { stack = stack1, output = [ reverseContents block1 ] }

                Just ( block2, stack2 ) ->
                    if blockLevel block1 <= blockLevel block2 then
                        let
                            _ =
                                debug1 "blockLevel blockM1 <= blockLevel blockM2" True
                        in
                        state

                    else
                        case block2 of
                            Block name blocks meta ->
                                let
                                    _ =
                                        debug1 "reduceStack Block" True

                                    newBlock : Block
                                    newBlock =
                                        Block name (reverseContents block1 :: blocks) meta
                                in
                                reduceStack_ { state | stack = stack2, output = newBlock :: state.output }

                            VerbatimBlock name blocks meta ->
                                case block1 of
                                    Paragraph strings metaP ->
                                        let
                                            newBlock : Block
                                            newBlock =
                                                VerbatimBlock name (List.reverse <| strings ++ blocks) meta
                                        in
                                        reduceStack_ { state | stack = stack2, output = newBlock :: state.output }

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
            case block of
                Paragraph strings meta ->
                    Paragraph (line :: strings) meta :: List.drop 1 stack

                _ ->
                    stack



-- SHIFT


replaceMeta : Syntax.Meta -> Block -> Block
replaceMeta meta block =
    case block of
        Paragraph strings _ ->
            Paragraph strings meta

        VerbatimBlock name strings _ ->
            VerbatimBlock name strings meta

        Block name blocks _ ->
            Block name blocks meta


shift : Block -> State -> State
shift block state =
    let
        newMeta =
            { start = state.lineNumber
            , end = state.lineNumber
            , indent = state.indent
            , id = String.fromInt state.generation ++ "." ++ String.fromInt state.blockCount
            }

        newBlock =
            replaceMeta newMeta block

        --{ content = block
        --, meta =
        --    Just
        --        { start = state.lineNumber
        --        , end = state.lineNumber
        --        , indent = state.indent
        --        , id = String.fromInt state.generation ++ "." ++ String.fromInt state.blockCount
        --        }
        --}
    in
    { state
        | stack = newBlock :: state.stack
        , lineNumber = state.lineNumber + 1
        , blockCount = state.blockCount + 1
    }
