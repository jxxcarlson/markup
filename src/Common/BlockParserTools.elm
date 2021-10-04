module Common.BlockParserTools exposing
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
    , nextStep
    , reduceStack
    , reduceStack_
    , reverseContents
    , reverseStack
    , shift
    , typeOfBlock
    )

import Common.BasicSyntax as Basic
import Common.Debug exposing (debug1, debug2, debug3)
import Common.Line exposing (LineType(..))
import Common.Syntax as Syntax exposing (Block(..), BlockType(..), dummyMeta)
import Dict
import List.Extra
import MiniLaTeX.MathMacro
import Utility


type alias State =
    { input : List String
    , output : List Block
    , indent : Int
    , verbatimBlockInitialIndent : Int
    , lineNumber : Int
    , generation : Int
    , blockCount : Int
    , inVerbatimBlock : Bool
    , counter : Int
    , accumulator : Accumulator
    , stack : List Block
    }


type alias Accumulator =
    { macroDict : MiniLaTeX.MathMacro.MathMacroDict }


nextStep : (String -> State -> State) -> State -> Step State State
nextStep nextStateAux state =
    let
        _ =
            debug2 "STACK" ( state.counter, List.map Basic.simplify state.stack, blockLevelOfStackTop state.stack )
    in
    case List.head state.input of
        Nothing ->
            let
                newState =
                    reduceStack { state | counter = state.counter + 1 }

                _ =
                    debug1 "!! STACK" ( newState.counter, List.map Basic.simplify newState.stack, blockLevelOfStackTop newState.stack )

                _ =
                    debug1 "!! Reduce stack" (newState.output |> List.map Basic.simplify)

                finalState =
                    { newState | output = newState.stack ++ newState.output |> List.reverse |> debug1 "reverse output (2)" }

                _ =
                    finalState |> .output |> List.map Basic.simplify |> debug1 "OUTPUT"
            in
            Done finalState

        Just line ->
            Loop (nextStateAux line { state | counter = state.counter + 1, input = List.drop 1 state.input })


initialState : Int -> List String -> State
initialState generation input =
    { input = input
    , output = []
    , indent = 0
    , verbatimBlockInitialIndent = 0
    , lineNumber = 0
    , generation = generation
    , blockCount = 0
    , counter = 0
    , inVerbatimBlock = False
    , accumulator = initialAccumulator
    , stack = []
    }


initialAccumulator =
    { macroDict = Dict.empty }


reduceStack : State -> State
reduceStack state =
    case state.stack of
        block1 :: block2 :: rest ->
            if blockLevel block1 <= blockLevel block2 then
                state

            else
                reduceStackAux state rest block1 block2

        block1 :: rest ->
            { state | output = reverseContents block1 :: state.output, stack = rest }

        [] ->
            state


reduceStackAux : State -> List Block -> Block -> Block -> State
reduceStackAux state stack2 block1 block2 =
    case block2 of
        Block name blocks meta ->
            let
                newBlock : Block
                newBlock =
                    -- (2)
                    Block name (reverseContents block1 :: blocks) meta
            in
            -- (3) TODO: double counting???
            reduceStack { state | stack = stack2, output = reverseContents newBlock :: state.output }

        VerbatimBlock name blocks meta ->
            case block1 of
                Paragraph strings metaP ->
                    let
                        newBlock : Block
                        newBlock =
                            -- TODO: (List.reverse strings ++ blocks) => (strings ++ blocks)
                            VerbatimBlock name (strings ++ blocks) metaP
                    in
                    -- (4)
                    reduceStack { state | stack = stack2, output = reverseContents newBlock :: state.output }

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

        BlockError _ ->
            0


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

        BlockError _ ->
            E


blockLabel : Block -> String
blockLabel block =
    case block of
        Paragraph _ _ ->
            "(no label)"

        Block s _ _ ->
            s

        VerbatimBlock s _ _ ->
            s

        BlockError s ->
            s


blockLabelAtBottomOfStack : List Block -> String
blockLabelAtBottomOfStack stack =
    case List.head (List.reverse stack) of
        Nothing ->
            "(no label)"

        Just block ->
            blockLabel block


reverseContents : Block -> Block
reverseContents block =
    (let
        _ =
            debug3 "THE REAL reverseContents (IN)" block
     in
     case block of
        Paragraph strings meta ->
            Paragraph (List.reverse strings) meta

        VerbatimBlock name strings meta ->
            VerbatimBlock name (List.reverse strings) meta

        Block name blocks meta ->
            -- (7)
            Block name (List.map reverseContents blocks) meta

        BlockError s ->
            BlockError s
    )
        |> debug3 "THE REAL reverseContents (OUT)"



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
                    -- (5)
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
                                        -- (6)
                                        Block name (reverseContents block1 :: blocks) meta
                                in
                                reduceStack_ { state | stack = stack2, output = newBlock :: state.output }

                            VerbatimBlock name blocks meta ->
                                case block1 of
                                    Paragraph strings metaP ->
                                        let
                                            newBlock : Block
                                            newBlock =
                                                VerbatimBlock name (List.reverse <| strings ++ blocks) meta |> debug3 "NEW BLOCK"
                                        in
                                        reduceStack_ { state | stack = stack2, output = newBlock :: state.output }

                                    _ ->
                                        state

                            _ ->
                                state


appendLineAtTop : String -> List Block -> List Block
appendLineAtTop line stack =
    -- TODO: check this
    case List.head stack of
        Nothing ->
            stack

        Just block ->
            case block of
                -- TODO: fix Meta, examine other cases not handled
                Paragraph strings meta ->
                    Paragraph (line :: strings) meta :: List.drop 1 stack

                Block name ((Paragraph lines meta1) :: rest) meta2 ->
                    Block name (Paragraph (line :: lines) meta1 :: rest) meta2 :: List.drop 1 stack

                Block name [] meta2 ->
                    Block name [ Paragraph [ line ] (dummyMeta 0 0) ] meta2 :: List.drop 1 stack

                Block name ((Block name2 blocks meta1) :: rest) meta2 ->
                    Block name (Paragraph [ line ] meta1 :: Block name2 blocks meta1 :: rest) meta2 :: List.drop 1 stack

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

        BlockError s ->
            BlockError s


shift : Block -> State -> State
shift block state =
    let
        newMeta =
            { begin = state.lineNumber
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



-- NEXT
