module Chunker exposing (run)

import L1.Line as Line
import Line exposing (LineType(..))


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


run : Int -> List String -> List BlockM
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


nextState : State -> Step State (List BlockM)
nextState state =
    case List.head state.input of
        Nothing ->
            Done state.output

        Just line ->
            Loop (nextStateAux line { state | input = List.drop 1 state.input })


indentQuantum =
    3


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
            shift (Block s []) { state | indent = indent }

        BeginVerbatimBlock s ->
            shift (VerbatimBlock s []) { state | indent = indent }

        OrdinaryLine ->
            handleOrdinaryLine indent line state

        _ ->
            -- TODO: finish up
            state


handleOrdinaryLine indent line state =
    if indent > state.indent + indentQuantum then
        shift (Paragraph [ line ]) { state | indent = indent }

    else if indent < state.indent - indentQuantum then
        -- Indented less
        state |> reduce |> shift (Paragraph [ line ]) |> (\st -> { st | indent = indent })

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
    -- TODO: implement this
    stack



-- Indented about the same


reduce : State -> State
reduce state =
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


loop : State -> (State -> Step State (List BlockM)) -> List BlockM
loop s nextState_ =
    case nextState s of
        Loop s_ ->
            loop s_ nextState_

        Done b ->
            b



--lineType : Int -> String -> LineType
--lineType indent str =
--
