module BlockParser exposing (Block(..), run, runFromString)

import Console
import L1.Line as Line
import Line exposing (LineType(..))
import List.Extra


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


type Block
    = Paragraph (List String)
    | VerbatimBlock String (List String)
    | Block String (List Block)


type BlockType
    = P
    | V
    | B


typeOfBlock : Block -> BlockType
typeOfBlock b =
    case b of
        Paragraph _ ->
            P

        VerbatimBlock _ _ ->
            V

        Block _ _ ->
            B


type alias BlockM =
    { content : Block, meta : Maybe Meta }


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
                { state | indent = indent } |> reduceStack |> shift (VerbatimBlock s [])

            else
                { state | indent = indent } |> shift (VerbatimBlock s [])

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
        state


handleOrdinaryLine indent line state =
    if level indent >= blockLevelOfStackTop state.stack then
        let
            _ =
                debug3 "BRANCH 3" "Shift or append line at top"
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
                debug3 "BRANCH 1" "Shift, reduceStack"
        in
        shift (Paragraph [ line ]) (reduceStack { state | indent = indent })

    else
        -- else if level indent > blockLevelOfStackTop state.stack then
        let
            _ =
                debug3 "BRANCH 2" "INACCESSIBLE ??"
        in
        state |> reduce OrdinaryLine |> shift (Paragraph [ String.dropLeft indent line ]) |> (\st -> { st | indent = indent })


reverseStack : State -> State
reverseStack state =
    { state | stack = List.reverse state.stack }


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
                    { state | output = reverseContentsM blockM1 :: state.output, stack = [] }

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
                                let
                                    _ =
                                        state.stack ++ state.output |> debug1 "STATE IN reduceStack"
                                in
                                { state | output = state.stack ++ state.output }



--{ state | stack = newBlock :: stack3 }


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



-- Indented about the same


reduce : Line.LineType -> State -> State
reduce lineType state =
    state


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


type alias Meta =
    { start : Int
    , end : Int
    , indent : Int
    , id : String
    }


type Step state a
    = Loop state
    | Done a


loop : State -> (State -> Step State State) -> State
loop s nextState_ =
    case nextState s of
        Loop s_ ->
            loop s_ nextState_

        Done b ->
            b



-- HELPERS


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
