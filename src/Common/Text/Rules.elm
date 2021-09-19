module Common.Text.Rules exposing (..)

import Dict exposing (Dict)


type alias ActionItem =
    { name : String
    , start : Char -> Bool
    , expect : List { stop : Char -> Bool, action : Action }
    }


type alias ActionItems =
    List ActionItem


type Action
    = CommitText
    | CommitMarked
    | ShiftMarked
    | ShiftArg


miniLaTexDelimiters =
    [ '\\', '{', '}' ]


type alias Rule =
    { name : String
    , start : Char -> Bool
    , expect : List { stop : Char -> Bool, action : Action }
    }


defaultRule : Rule
defaultRule =
    { name = "alpha"
    , start = \c -> not (List.member c (' ' :: miniLaTexDelimiters))
    , expect = [ { stop = \c -> List.member c miniLaTexDelimiters, action = CommitText } ]
    }


getRule : { dict : Dict Char Rule, default : Rule } -> Char -> Rule
getRule ruleData char =
    case Dict.get char ruleData.dict of
        Nothing ->
            ruleData.default

        Just rule ->
            rule


miniLaTeXRuleDict : Dict Char Rule
miniLaTeXRuleDict =
    Dict.fromList miniLaTeXRules


miniLaTeXRules =
    [ ( '\\'
      , { name = "macro"
        , start = \c -> c == '\\'
        , expect =
            [ { stop = \c -> c == ' ', action = CommitMarked }
            , { stop = \c -> c == '{', action = ShiftMarked }
            ]
        }
      )
    , ( ' '
      , { name = "blank"
        , start = \c -> c == ' '
        , expect =
            [ { stop = \c -> not (List.member c miniLaTexDelimiters), action = CommitText }
            ]
        }
      )
    , ( '{'
      , { name = "argBegin"
        , start = \c -> c == '{'
        , expect =
            [ { stop = \c -> c == '}', action = ShiftArg }
            ]
        }
      )
    ]
