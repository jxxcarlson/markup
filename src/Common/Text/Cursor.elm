module Common.Text.Cursor exposing (Step(..), TextCursor, init, nextCursor)

import Common.Library.ParserTools as ParserTools
import Common.Syntax as Syntax exposing (Text(..))
import Common.Text.Configuration as Configuration exposing (Configuration)
import Common.Text.Error exposing (Context(..), Problem(..))
import Common.Text.Rule as Rule exposing (Action(..), Rule, Rules)
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
    , committed : List Text
    , stack : List Text
    , generation : Int
    , count : Int
    }


init : Int -> Int -> Int -> String -> TextCursor
init generation count scanPoint str =
    { scanPoint = scanPoint
    , scannerType = NormalScan
    , source = str
    , stringData = { content = "", start = 0, finish = 0 }
    , committed = []
    , stack = []
    , generation = generation
    , count = count
    }


type ScannerType
    = NormalScan
    | VerbatimScan Char


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
                        _ =
                            Debug.log "stringData.content" stringData.content

                        scanPoint =
                            cursor.scanPoint + stringData.finish - stringData.start

                        stopStr =
                            String.slice scanPoint (scanPoint + 1) cursor.source

                        action =
                            Rule.getAction stopStr rule

                        meta =
                            { start = cursor.scanPoint
                            , end = cursor.scanPoint + stringData.finish - stringData.start
                            , indent = 0
                            , id = String.fromInt cursor.generation ++ "." ++ String.fromInt cursor.count
                            }

                        ( committed, stack ) =
                            case action of
                                CommitText ->
                                    ( Text [ stringData.content ] meta :: cursor.committed, cursor.stack )

                                CommitMarked ->
                                    ( Marked (String.dropLeft 1 stringData.content) [] meta :: cursor.committed, cursor.stack )

                                ShiftMarked ->
                                    ( cursor.committed, Marked (String.dropLeft 1 stringData.content) [] meta :: cursor.stack )

                                _ ->
                                    ( cursor.committed, cursor.stack )

                        _ =
                            ( scanPoint, stopStr, action ) |> Debug.log "(ScanPoint, StopStr, Action)"
                    in
                    Loop
                        { cursor
                            | stringData = stringData
                            , committed = committed
                            , stack = stack
                            , scanPoint = scanPoint
                            , count = cursor.count + 1
                        }



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
