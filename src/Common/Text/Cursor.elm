module Common.Text.Cursor exposing (Step(..), TextCursor, advance, init, nextCursor)

import Common.Library.ParserTools as ParserTools
import Common.Syntax as Syntax exposing (Text(..))
import Common.Text.Configuration as Configuration exposing (Configuration)
import Common.Text.Error exposing (Context(..), Problem(..))
import Common.Text.Rule as Rule exposing (Rule, Rules)
import Dict exposing (Dict)
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


nextCursor : Rules -> TextCursor -> Step TextCursor TextCursor
nextCursor rules cursor =
    let
        textToProcess : String
        textToProcess =
            String.dropLeft cursor.scanPoint cursor.source
    in
    case String.uncons textToProcess of
        Nothing ->
            Done cursor

        Just ( leadingChar, restOfText ) ->
            let
                rule =
                    Rule.get rules leadingChar

                _ =
                    rule.name |> Debug.log "RULE"
            in
            case ParserTools.getText rule.start rule.continue textToProcess of
                Err _ ->
                    Done cursor

                Ok stringData ->
                    let
                        scanPoint =
                            cursor.scanPoint + stringData.finish - stringData.start

                        stopStr =
                            String.slice scanPoint (scanPoint + 1) cursor.source

                        action =
                            Rule.getAction stopStr rule

                        _ =
                            ( scanPoint, stopStr, action ) |> Debug.log "(ScanPoint, StopStr, Action)"
                    in
                    Loop { cursor | stringData = stringData, scanPoint = scanPoint }



--case stringData of
--
--Loop { cursor | scanPoint = cursor.scanPoint + stringData.finish - stringData.start }
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
