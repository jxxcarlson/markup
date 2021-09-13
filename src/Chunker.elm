module Chunker exposing (Chunk, ChunkType(..), run)

import L1.Line as Line


type Chunk
    = Chunk
        { start : Int
        , end : Int
        , indent : Int
        , level : Int
        , id : String
        , chunkType : ChunkType
        , generation : Int
        , content : List String
        , children : List Chunk
        }


type ChunkType
    = Paragraph
    | Verbatim String
    | Block String


type alias State =
    { input : List String
    , output : List Chunk
    , indent : Int
    , stack : List Chunk
    }


run : List String -> List Chunk
run input =
    loop (initialState input) nextState


initialState : List String -> State
initialState input =
    { input = input, output = [], indent = 0, stack = [] }


nextState : State -> Step State (List Chunk)
nextState state =
    case List.head state.input of
        Nothing ->
            Done state.output

        Just line ->
            Loop (nextStateAux line { state | input = List.drop 1 state.input })


nextStateAux : String -> State -> State
nextStateAux line state =
    let
        lineType =
            Line.classify line
    in
    --case lineType of
    state


type Step state a
    = Loop state
    | Done a


loop : State -> (State -> Step State (List Chunk)) -> List Chunk
loop s nextState_ =
    case nextState s of
        Loop s_ ->
            loop s_ nextState_

        Done b ->
            b



--lineType : Int -> String -> LineType
--lineType indent str =
--
