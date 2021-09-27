module Common.Text.Cursor exposing (Step(..), TextCursor, init, nextCursor, parseLoop)

import Common.Debug exposing (debug1, debug2, debug3)
import Common.Library.ParserTools as ParserTools exposing (StringData)
import Common.Syntax as Syntax exposing (Text(..))
import Common.Text
import Common.Text.Error exposing (Context(..), Problem(..))
import Common.Text.Reduce as Reduce
import Common.Text.Rule as Rule exposing (Action(..), Rule, Rules)
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
            -- We need to apply contractStackRepeatedly when there is nothing left to process
            -- so as to clear the stack (assuming no errors)
            -- Example (Markdown cursor test (1):
            --    STACK: [Text "Introduction to Chemistry" META ,Marked "title" [] META]
            -- TODO: this should be done less crudely
            case List.Extra.uncons (Reduce.contractStackRepeatedly cursor.stack) of
                Nothing ->
                    Done cursor

                Just ( item, rest ) ->
                    Done { cursor | committed = Common.Text.reverse item :: cursor.committed, stack = rest }

        Just ( leadingChar, _ ) ->
            -- NOTE: use rules here
            nextCursor_ leadingChar cursor rules textToProcess


nextCursor_ leadingChar cursor rules textToProcess =
    let
        -- NOTE: use rules here
        -- Especially in getParser
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
                    -- NOTE: use rules here
                    cursor.scanPoint + stringData.finish - stringData.start + rule.endCharLength

                stopStr =
                    String.slice scanPoint (scanPoint + 1) cursor.source

                action =
                    -- NOTE: use rules here
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
                                    -- AGAIN,
                                    -- EXAMPLE, Markdown cursor test (2), "It was *very* bold" yields
                                    --     [Text "It was " META, Marked "strong" [] META,Text " " META ,Marked "strong" [] META]
                                    -- INSTEAD OF
                                    --     [Text "It was " META,Marked "strong" [Text "very" META] META,Text " " META,Text "bold" META]
                                    Reduce.contractStackRepeatedly cursor.stack |> debug3 "newStack (Commit)"

                                -- TODO: not working for \\foo{\\bar{baz}
                                -- TODO: we have to handle the case of length newStack > 1, which is an error state
                            in
                            case List.head newStack of
                                Just item ->
                                    ( Text " " (Syntax.dummyMeta 0 0) :: (Common.Text.reverse item |> debug2 "Common.Text.reverse") :: cursor.committed, List.drop 1 newStack )

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

                        ShiftText2 ->
                            let
                                textList =
                                    String.words stringData.content |> List.map (\s -> Text (s ++ " ") meta)
                            in
                            if cursor.stack == [] then
                                ( Common.Text.combine textList ++ cursor.committed, cursor.stack )

                            else
                                ( cursor.committed, textList ++ cursor.stack )

                        ShiftMarked ->
                            --  ( cursor.committed, Marked (String.trim stringData.content) [] meta :: cursor.stack )
                            let
                                mark =
                                    -- NOTE: use rules here
                                    String.dropLeft rule.dropLeadingChars stringData.content |> String.trimRight |> rule.transform

                                _ =
                                    debug2 "ShiftMarked mark, (drop, before, after)" ( rule.dropLeadingChars, stringData.content, String.dropLeft rule.dropLeadingChars stringData.content |> String.trimRight )
                            in
                            ( cursor.committed, Marked mark [] meta :: cursor.stack )

                        ShiftVerbatim c ->
                            ( cursor.committed, Verbatim c "" meta :: cursor.stack |> Reduce.contract3Stack )

                        ShiftArg ->
                            ( cursor.committed, Arg [] meta :: cursor.stack )

                        ReduceArg ->
                            let
                                _ =
                                    debug3 "ReduceArg (IN)" cursor.stack
                            in
                            ( cursor.committed, (Reduce.markedIntoMarked >> Reduce.textIntoMarked >> Reduce.textIntoArg >> Reduce.argIntoMarked >> Reduce.markedIntoArg) cursor.stack |> debug3 "ReduceArg (OUT)" )

                        ReduceArgList ->
                            ( cursor.committed, Reduce.argList cursor.stack )

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
