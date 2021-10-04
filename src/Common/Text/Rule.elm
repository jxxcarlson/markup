module Common.Text.Rule exposing (Action(..), ParseEnd(..), Rule, Rules, get, getAction)

import Dict exposing (Dict)


type alias Rule =
    { name : String
    , start : Char -> Bool
    , continue : Char -> Bool
    , parseEnd : ParseEnd
    , endCharLength : Int
    , dropLeadingChars : Int
    , isVerbatim : Bool
    , transform : String -> String
    , expect : List { stop : List String, action : Action }
    }


type ParseEnd
    = EndNormal
    | EndEatSpace
    | EndEatSymbol String


type Action
    = Commit
    | CommitMarked
    | ShiftText
    | ShiftText2
    | ShiftMarked
    | ShiftVerbatim String
    | ShiftVerbatim2 String
    | ShiftArg
    | ReduceArg
    | ReduceArgList
    | ErrorAction


getAction : String -> Rule -> Action
getAction stopStr rule =
    case rule.expect of
        data :: [] ->
            data.action

        [] ->
            ErrorAction

        _ ->
            List.filter (\item -> List.member stopStr item.stop) rule.expect |> List.head |> Maybe.map .action |> Maybe.withDefault ErrorAction


type alias Rules =
    { dict : Dict Char Rule, default : Rule }


get : Rules -> Char -> Rule
get ruleData char =
    case Dict.get char ruleData.dict of
        Nothing ->
            ruleData.default

        Just rule ->
            rule
