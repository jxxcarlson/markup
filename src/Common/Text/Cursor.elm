module Common.Text.Cursor exposing (Step(..), TextCursor, init, nextCursor, parseLoop)

import Common.Debug exposing (debug1, debug2, debug3)
import Common.Library.ParserTools as ParserTools
import Common.Syntax as Syntax exposing (Text(..))
import Common.Text.Configuration as Configuration exposing (Configuration)
import Common.Text.Error exposing (Context(..), Problem(..))
import Common.Text.Rule as Rule exposing (Action(..), Rule, Rules)
import Dict exposing (Dict)
import List.Extra
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


parseLoop : Rules -> TextCursor -> TextCursor
parseLoop rules initialCursor =
    loop initialCursor (nextCursor rules)


nextCursor : Rules -> TextCursor -> Step TextCursor TextCursor
nextCursor rules cursor =
    let
        textToProcess : String
        textToProcess =
            String.dropLeft cursor.scanPoint cursor.source
    in
    case String.uncons textToProcess of
        Nothing ->
            let
                stack =
                    contractStackRepeatedly cursor.stack
            in
            case List.Extra.uncons stack of
                Nothing ->
                    Done cursor

                Just ( item, rest ) ->
                    Done { cursor | committed = item :: cursor.committed, stack = rest }

        Just ( leadingChar, restOfText ) ->
            let
                rule =
                    Rule.get rules leadingChar

                _ =
                    rule.name |> debug1 "RULE"
            in
            case ParserTools.getText rule.start rule.continue textToProcess of
                Err _ ->
                    Done cursor

                Ok stringData ->
                    let
                        _ =
                            Debug.log "stringData.content" stringData.content

                        scanPoint =
                            cursor.scanPoint + stringData.finish - stringData.start + rule.endCharLength

                        stopStr =
                            String.slice scanPoint (scanPoint + 1) cursor.source

                        action =
                            Rule.getAction stopStr rule

                        meta =
                            { start = cursor.scanPoint
                            , end = cursor.scanPoint + stringData.finish - stringData.start + rule.endCharLength
                            , indent = 0
                            , id = String.fromInt cursor.generation ++ "." ++ String.fromInt cursor.count
                            }

                        ( committed, stack ) =
                            case action of
                                CommitText ->
                                    ( Text stringData.content meta :: cursor.committed, cursor.stack )

                                CommitMarked ->
                                    ( Marked (String.dropLeft 1 stringData.content) [] meta :: cursor.committed, cursor.stack )

                                ShiftText ->
                                    if cursor.stack == [] then
                                        ( Text stringData.content meta :: cursor.committed, cursor.stack )

                                    else
                                        ( cursor.committed, Text stringData.content meta :: cursor.stack )

                                ShiftMarked ->
                                    ( cursor.committed, Marked (String.dropLeft 1 stringData.content) [] meta :: cursor.stack )

                                ShiftArg ->
                                    ( cursor.committed, Arg [] meta :: cursor.stack )

                                ReduceArg ->
                                    let
                                        _ =
                                            debug2 "ReduceArg, contracted stack" contractStack cursor.stack
                                    in
                                    ( cursor.committed, contractStack cursor.stack )

                                _ ->
                                    ( cursor.committed, cursor.stack )

                        _ =
                            debug2 "STACK" stack

                        _ =
                            debug2 "COMMITTED" committed

                        _ =
                            ( scanPoint, stopStr, action ) |> Debug.log "(ScanPoint, StopStr, Action)"
                    in
                    Loop
                        { cursor
                            | stringData = stringData
                            , committed = committed
                            , stack = stack
                            , scanPoint = scanPoint |> Debug.log "scanPoint"
                            , count = cursor.count + 1
                        }


contract : Text -> Text -> Maybe Text
contract text1 text2 =
    case ( text1, text2 ) of
        ( Arg textList1 meta1, Arg textList2 meta2 ) ->
            Just <| Arg (textList1 ++ textList2) { start = meta2.start, end = meta1.end, indent = 0, id = meta2.id }

        ( Arg textList1 meta1, Marked name textList2 meta2 ) ->
            Just <| Marked name (textList1 ++ textList2) { start = meta2.start, end = meta1.end, indent = 0, id = meta2.id }

        ( Text str meta1, Arg textList2 meta2 ) ->
            Just <| Arg (Text str meta1 :: textList2) { start = meta2.start, end = meta1.end, indent = 0, id = meta2.id }

        ( Marked name textList1 meta1, Arg textList2 meta2 ) ->
            Just <| Arg (Marked name textList1 meta1 :: textList2) { start = meta2.start, end = meta1.end, indent = 0, id = meta2.id }

        ( _, _ ) ->
            Nothing


contractStack : List Text -> List Text
contractStack stack =
    case stack of
        text1 :: text2 :: rest ->
            case contract text1 text2 of
                Nothing ->
                    stack

                Just text3 ->
                    let
                        _ =
                            Debug.log "ACTION" "contract stack, scanPoint"
                    in
                    text3 :: rest

        _ ->
            stack


contractStackRepeatedly : List Text -> List Text
contractStackRepeatedly stack =
    case stack of
        text1 :: text2 :: rest ->
            case contract text1 text2 of
                Nothing ->
                    stack

                Just text3 ->
                    let
                        _ =
                            Debug.log "ACTION" "contract stack, scanPoint"
                    in
                    contractStackRepeatedly (text3 :: rest)

        _ ->
            stack



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
