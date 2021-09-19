module Common.Text.Cursor exposing (TextCursor, advance, init)

import Common.Library.ParserTools as ParserTools
import Common.Syntax as Syntax exposing (Text(..))
import Common.Text.Configuration as Configuration exposing (Configuration)
import Parser.Advanced


{-|

    - scanPoint: Int = an index into source: String
    - scannerType: takes values of ScannerType, e.g. `NormalScan` or `VerbatimSca '$'`.
    - source: the source text
    - parsed: List of already parsed items
    - stack: List of partially parsed items

-}
type alias TextCursor =
    { scanPoint : Int
    , scannerType : ScannerType
    , source : String
    , stringData : ParserTools.StringData
    , parsed : List Text
    , stack : List Text
    }


init : Int -> String -> TextCursor
init scanPoint str =
    { scanPoint = scanPoint
    , scannerType = NormalScan
    , source = str
    , stringData = { content = "", start = 0, finish = 0 }
    , parsed = []
    , stack = []
    }


type ScannerType
    = NormalScan
    | VerbatimScan Char


{-|

    Find the longest prefix of the source text beginning at the scan point
    that does not contain any delimiters. Store that prefix and its start
    and finish positions in cursor.stringData and advance the scanPointer
    by the length of the prefix found.

-}
advance : Configuration -> TextCursor -> TextCursor
advance config cursor =
    let
        textToProcess =
            String.dropLeft cursor.scanPoint cursor.source

        predicate =
            case cursor.scannerType of
                NormalScan ->
                    \c -> not (List.member c config.delimiters)

                VerbatimScan verbatimChar ->
                    \c -> c /= verbatimChar
    in
    case ParserTools.getText predicate predicate textToProcess of
        Err _ ->
            cursor

        Ok stringData ->
            { cursor | stringData = stringData |> Debug.log "STRING DATA", scanPoint = cursor.scanPoint + stringData.finish - stringData.start }



-- LOOP


loop : state -> (state -> Step state a) -> a
loop s nextState =
    case nextState s of
        Loop s_ ->
            loop s_ nextState

        Done b ->
            b


type Step state a
    = Loop state
    | Done a
