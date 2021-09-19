module Common.Text.Rule exposing (Action(..), Rule, Rules, get, getAction, miniLaTeXRules)

import Dict exposing (Dict)


type Action
    = CommitText
    | CommitMarked
    | ShiftMarked
    | ShiftArg
    | ErrorAction


type alias Rule =
    { name : String
    , start : Char -> Bool
    , continue : Char -> Bool
    , expect : List { stop : List String, action : Action }
    }


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


miniLaTeXRules : Rules
miniLaTeXRules =
    { dict = miniLaTeXRuleDict, default = defaultRule }


get : Rules -> Char -> Rule
get ruleData char =
    case Dict.get char ruleData.dict of
        Nothing ->
            ruleData.default

        Just rule ->
            rule


miniLaTeXRuleDict : Dict Char Rule
miniLaTeXRuleDict =
    Dict.fromList miniLaTeXRuleList


defaultRule : Rule
defaultRule =
    { name = "alpha"
    , start = \c -> not (List.member c (' ' :: miniLaTexDelimiters))
    , continue = \c -> not (List.member c miniLaTexDelimiters)
    , expect = [ { stop = miniLaTexDelimitersStr, action = CommitText } ]
    }


miniLaTexDelimiters =
    [ '\\', '{', '}' ]


miniLaTexDelimitersStr =
    [ "\\", "{", "}" ]


miniLaTeXRuleList =
    [ ( '\\'
      , { name = "macro"
        , start = \c -> c == '\\'
        , continue = \c -> not (c == ' ' || c == '{')
        , expect =
            [ { stop = [ " ", "" ], action = CommitMarked }
            , { stop = [ "{" ], action = ShiftMarked }
            ]
        }
      )
    , ( ' '
      , { name = "blank"
        , start = \c -> c == ' '
        , continue = \c -> not (List.member c miniLaTexDelimiters)
        , expect =
            [ { stop = miniLaTexDelimitersStr, action = CommitText }
            ]
        }
      )
    , ( '{'
      , { name = "argBegin"
        , start = \c -> c == '{'
        , continue = \c -> c /= '}'
        , expect =
            [ { stop = [ "}" ], action = ShiftArg }
            ]
        }
      )
    ]
