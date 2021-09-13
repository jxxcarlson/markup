module Chunker exposing (Chunk, ChunkType(..))


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



--lineType : Int -> String -> LineType
--lineType indent str =
--
