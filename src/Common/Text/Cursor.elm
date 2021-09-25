module Common.Text.Cursor exposing (Step(..), TextCursor, init, nextCursor, parseLoop)

import Common.Debug exposing (debug1, debug2, debug3)
import Common.Library.ParserTools as ParserTools exposing (StringData)
import Common.Syntax as Syntax exposing (Text(..))
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
    let
        _ =
            debug3 "+++++++++++++++" "+"
    in
    if initialCursor.source == "" then
        { initialCursor
            | stack = []
            , committed = [ Text "" { start = initialCursor.scanPoint, end = initialCursor.scanPoint, indent = 0, id = String.fromInt initialCursor.generation } ]
        }

    else
        loop initialCursor (nextCursor rules)


nextCursor : Rules -> TextCursor -> Step TextCursor TextCursor
nextCursor rules cursor =
    let
        textToProcess =
            String.dropLeft cursor.scanPoint cursor.source
    in
    case String.uncons textToProcess of
        Nothing ->
            case List.Extra.uncons (contractStackRepeatedly cursor.stack) of
                Nothing ->
                    Done cursor

                Just ( item, rest ) ->
                    Done { cursor | committed = item :: cursor.committed, stack = rest }

        Just ( leadingChar, _ ) ->
            nextCursor_ leadingChar cursor rules textToProcess


nextCursor_ leadingChar cursor rules textToProcess =
    let
        rule =
            Rule.get rules leadingChar

        scannerType =
            getScannerType cursor rule leadingChar

        currentParser =
            getParser rule

        _ =
            debug3 "========================" "="

        _ =
            rule.name |> debug1 "RULE"
    in
    case currentParser textToProcess of
        Err _ ->
            Done cursor

        Ok stringData ->
            let
                _ =
                    debug1 "stringData.content" stringData.content

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
                        Commit ->
                            let
                                newStack =
                                    contractStackRepeatedly cursor.stack |> debug3 "newStack (Commit)"

                                -- TODO: not working for \\foo{\\bar{baz}
                                -- TODO: we have to handle the case of length newStack > 1, which is an error state
                            in
                            case List.head newStack of
                                Just item ->
                                    ( Text " " (Syntax.dummyMeta 0 0) :: item :: cursor.committed, List.drop 1 newStack )

                                Nothing ->
                                    -- TODO: is this correct?
                                    ( cursor.committed, [] )

                        CommitMarked ->
                            ( Marked (String.dropLeft 1 stringData.content) [] meta :: cursor.committed, cursor.stack )

                        ShiftText ->
                            if cursor.stack == [] then
                                ( Text stringData.content meta :: cursor.committed, cursor.stack )

                            else
                                ( cursor.committed, Text stringData.content meta :: cursor.stack )

                        ShiftMarked ->
                            --  ( cursor.committed, Marked (String.trim stringData.content) [] meta :: cursor.stack )
                            let
                                _ =
                                    debug2 "ShiftMarked mark, (drop, before, after)" ( rule.dropLeadingChars, stringData.content, String.dropLeft rule.dropLeadingChars stringData.content |> String.trimRight )
                            in
                            ( cursor.committed, Marked (String.dropLeft rule.dropLeadingChars stringData.content |> String.trimRight) [] meta :: cursor.stack )

                        ShiftVerbatim c ->
                            ( cursor.committed, Verbatim c "" meta :: cursor.stack |> contract3Stack )

                        ShiftArg ->
                            ( cursor.committed, Arg [] meta :: cursor.stack )

                        ReduceArg ->
                            let
                                _ =
                                    debug2 "ReduceArg, contractStackRepeatedly stack" (contractStackRepeatedly cursor.stack)
                            in
                            ( cursor.committed, (contractTextIntoArg >> contractArgIntoMarked >> contractMarkedIntoArg) cursor.stack )

                        _ ->
                            ( cursor.committed, cursor.stack )

                _ =
                    debug2 "STACK" stack

                _ =
                    debug2 "COMMITTED" committed

                _ =
                    ( scanPoint, stopStr, action ) |> debug1 "(ScanPoint, StopStr, Action)"
            in
            Loop
                { cursor
                    | stringData = stringData
                    , committed = committed
                    , stack = stack
                    , scanPoint = scanPoint |> debug1 "scanPoint"
                    , count = cursor.count + 1
                }


getParser : Rule -> String -> Result (List (Parser.Advanced.DeadEnd Context Problem)) StringData
getParser rule =
    if rule.spaceFollows then
        ParserTools.getTextAndSpaces rule.start rule.continue

    else
        ParserTools.getText rule.start rule.continue


getScannerType : TextCursor -> Rule -> Char -> ScannerType
getScannerType cursor rule leadingChar =
    case cursor.scannerType of
        NormalScan ->
            if rule.isVerbatim then
                VerbatimScan leadingChar

            else
                NormalScan

        VerbatimScan c ->
            if c == leadingChar then
                NormalScan

            else
                VerbatimScan c


contractMarkedIntoArg : List Text -> List Text
contractMarkedIntoArg stack =
    case stack of
        (Marked name textList1 meta1) :: (Arg textList2 meta2) :: rest ->
            let
                _ =
                    debug2 "contractMarkedIntoArg" 3
            in
            Arg [ Marked name textList1 meta1 ] { start = meta2.start, end = meta1.end, indent = 0, id = meta2.id } :: rest

        _ ->
            stack


contractTextIntoArg : List Text -> List Text
contractTextIntoArg stack =
    case stack of
        (Text str meta1) :: (Arg textList2 meta2) :: rest ->
            let
                _ =
                    debug2 "contractTextIntoArg" 3
            in
            Arg (Text str meta1 :: textList2) { start = meta2.start, end = meta1.end, indent = 0, id = meta2.id } :: rest

        _ ->
            stack


contractArgIntoMarked : List Text -> List Text
contractArgIntoMarked stack =
    case stack of
        (Arg textList1 meta1) :: (Marked name textList2 meta2) :: rest ->
            let
                _ =
                    debug2 "contractArgIntoMarked" 2
            in
            Marked name (textList1 ++ textList2) { start = meta2.start, end = meta1.end, indent = 0, id = meta2.id } :: rest

        _ ->
            stack


contract : Text -> Text -> Maybe Text
contract text1 text2 =
    (let
        _ =
            debug3 "CONTRACT CASES (IN)" ( text1, text2 )
     in
     case ( text1, text2 ) of
        ( Arg textList1 meta1, Arg textList2 meta2 ) ->
            let
                _ =
                    debug2 "contract" 1
            in
            Just <| Arg (textList1 ++ textList2) { start = meta2.start, end = meta1.end, indent = 0, id = meta2.id }

        ( Arg textList1 meta1, Marked name textList2 meta2 ) ->
            let
                _ =
                    debug2 "contract" 2
            in
            Just <| Marked name (textList1 ++ textList2) { start = meta2.start, end = meta1.end, indent = 0, id = meta2.id }

        ( Text str meta1, Arg textList2 meta2 ) ->
            let
                _ =
                    debug2 "contract" 3
            in
            Just <| Arg (Text str meta1 :: textList2) { start = meta2.start, end = meta1.end, indent = 0, id = meta2.id }

        ( Marked name textList1 meta1, Arg textList2 meta2 ) ->
            let
                _ =
                    debug2 "contract" 4
            in
            Just <| Arg (Marked name textList1 meta1 :: textList2) { start = meta2.start, end = meta1.end, indent = 0, id = meta2.id }

        ( Text str meta1, Marked name textList2 meta2 ) ->
            let
                _ =
                    debug2 "contract" 5
            in
            Just <| Marked name (Text str meta1 :: textList2) { start = meta2.start, end = meta1.end, indent = 0, id = meta2.id }

        ( _, _ ) ->
            let
                _ =
                    debug2 "contract" 0
            in
            Nothing
    )
        |> debug3 "CONTRACT CASES (OUT)"


contract3 : Text -> Text -> Text -> Maybe Text
contract3 text1 text2 text3 =
    let
        _ =
            debug3 "contract3, stack" "-"
    in
    case ( text1, text2, text3 ) of
        ( Marked a [] meta1, _, Marked b [] meta3 ) ->
            if a == b then
                Just <| Marked a [ text2 ] { start = meta1.start, end = meta3.end, indent = 0, id = meta3.id }

            else
                Nothing

        ( Verbatim a "" meta1, Text x meta, Verbatim b "" meta3 ) ->
            if a == b then
                Just <| Verbatim a x { start = meta1.start, end = meta3.end, indent = 0, id = meta3.id }

            else
                Nothing

        _ ->
            Nothing


contract3Stack : List Text -> List Text
contract3Stack stack =
    let
        _ =
            debug3 "contractStack3, stack" stack
    in
    case stack of
        text1 :: text2 :: text3 :: rest ->
            case contract3 text1 text2 text3 of
                Nothing ->
                    stack

                Just text_ ->
                    let
                        _ =
                            debug1 "ACTION 3" "contract stack, scanPoint"
                    in
                    text_ :: rest

        _ ->
            stack


contractStack : List Text -> List Text
contractStack =
    let
        _ =
            debug2 "contractStack" "yes!"
    in
    contract2Stack >> contract3Stack


contract2Stack : List Text -> List Text
contract2Stack stack =
    let
        _ =
            debug3 "contractStack, stack" stack
    in
    case stack of
        text1 :: text2 :: rest ->
            case contract text1 text2 of
                Nothing ->
                    stack

                Just text_ ->
                    let
                        _ =
                            debug1 "ACTION 2" "contract stack, scanPoint"
                    in
                    text_ :: rest

        _ ->
            stack



--
--contractStackRepeatedly : List Text -> List Text
--contractStackRepeatedly stack =
--    stack |> contractStackRepeatedly


contractStackRepeatedly : List Text -> List Text
contractStackRepeatedly stack =
    let
        _ =
            debug1 "contractStackRepeatedly" "!!!"
    in
    (case stack of
        text1 :: text2 :: text3 :: rest ->
            case contract3 text1 text2 text3 of
                Nothing ->
                    stack

                Just text_ ->
                    let
                        _ =
                            debug1 "ACTION 3(R)" "contract stack, scanPoint"
                    in
                    text_ :: rest

        text1 :: text2 :: rest ->
            case contract text1 text2 of
                Nothing ->
                    stack

                Just text_ ->
                    let
                        _ =
                            debug1 "ACTION (2)R" "contract stack, scanPoint"
                    in
                    contractStackRepeatedly (text_ :: rest)

        _ ->
            stack
    )
        |> List.reverse



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
