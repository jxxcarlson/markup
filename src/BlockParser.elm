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
    , stack = []
    }


nextState : State -> Step State State
nextState state =
    let
        _ =
            debug1 "ST" state
    in
    case List.head state.input of
        Nothing ->
            Done (reduceStack state)

        Just line ->
            Loop (nextStateAux line { state | input = List.drop 1 state.input })


reduceStack : State -> State
reduceStack state =
    case List.Extra.uncons state.stack of
        Nothing ->
            state

        Just ( blockM, stack2 ) ->
            case List.Extra.uncons stack2 of
                Nothing ->
                    { state | output = blockM :: state.output, stack = [] }

                Just ( blockM2, stack3 ) ->
                    case blockM2.content of
                        Block name blocks ->
                            let
                                newBlock : BlockM
                                newBlock =
                                    { content = Block name (blockM.content :: blocks), meta = blockM2.meta }

                                newStack : List BlockM
                                newStack =
                                    newBlock :: stack3
                            in
                            reduceStack { state | stack = newStack }

                        _ ->
                            { state | output = state.stack ++ state.output }



--{ state | stack = newBlock :: stack3 }


indentQuantum =
    3


nextStateAux : String -> State -> State
nextStateAux line state =
    let
        lineType =
            Line.classify (line |> debug3 "Line")

        indent =
            lineType.indent
    in
    case lineType.lineType of
        BeginBlock s ->
            shift (Block s []) { state | indent = indent } |> debug2 ("Block " ++ s)

        BeginVerbatimBlock s ->
            shift (VerbatimBlock s []) { state | indent = indent } |> debug2 ("VerbatimBlock " ++ s)

        OrdinaryLine ->
            handleOrdinaryLine indent line state |> debug2 "OrdinaryLine"

        BlankLine ->
            handleBlankLine indent state |> debug2 "OrdinaryLine"

        EndBlock s ->
            -- TODO: finish up
            reduce (EndBlock s) state |> debug2 ("EndBlock " ++ s)

        EndVerbatimBlock s ->
            -- TODO: finish up
            reduce (EndVerbatimBlock s) state |> debug2 ("EndVerbatimBlock " ++ s)

        Problem s ->
            -- TODO: finish up
            state |> debug2 ("Problem " ++ s)


handleBlankLine indent state =
    -- TODO: finish up
    state


handleOrdinaryLine indent line state =
    if indent > state.indent + indentQuantum then
        shift (Paragraph [ line ]) { state | indent = indent }

    else if indent < state.indent - indentQuantum then
        -- Indented less
        state |> reduce OrdinaryLine |> shift (Paragraph [ line ]) |> (\st -> { st | indent = indent })

    else
        case List.head state.stack of
            Nothing ->
                shift (Paragraph [ line ]) { state | indent = indent }

            Just blockM ->
                if typeOfBlock blockM.content == P then
                    { state | stack = appendLineAtTop line state.stack, indent = indent }

                else
                    shift (Paragraph [ line ]) { state | indent = indent }


appendLineAtTop line stack =
    -- TODO: check this
    case List.head stack of
        Nothing ->
            stack

        Just block ->
            case block.content of
                Paragraph strings ->
                    { content = Paragraph (List.reverse (line :: strings)), meta = block.meta } :: List.drop 1 stack

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



--lineType : Int -> String -> LineType
--lineType indent str =
--
