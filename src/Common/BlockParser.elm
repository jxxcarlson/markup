module Common.BlockParser exposing
    ( State
    , Step(..)
    , appendLineAtTop
    , blockLabelAtBottomOfStack
    , blockLabelM
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
import Common.Syntax as Syntax exposing (Block(..), BlockM, BlockType(..))
import List.Extra


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


nextState : (String -> State -> State) -> State -> Step State State
nextState nextStateAux state =
    let
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
                    finalState |> .output |> List.map .content |> debug1 "OUTPUT"
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
