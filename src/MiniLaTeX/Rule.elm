module MiniLaTeX.Rule exposing (miniLaTeXRules)

import Common.Text.Rule exposing (Action(..), Rule, Rules)
import Dict exposing (Dict)


miniLaTeXRules : Rules
miniLaTeXRules =
    { dict = miniLaTeXRuleDict, default = defaultRule }


miniLaTeXRuleDict : Dict Char Rule
miniLaTeXRuleDict =
    Dict.fromList miniLaTeXRuleList


defaultRule : Rule
defaultRule =
    { name = "alpha"
    , start = \c -> not (List.member c (' ' :: miniLaTexDelimiters))
    , continue = \c -> not (List.member c miniLaTexDelimiters)
    , endCharLength = 0
    , expect = [ { stop = miniLaTexDelimitersStr, action = ShiftText } ]
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
        , endCharLength = 0
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
        , endCharLength = 0
        , expect =
            [ { stop = miniLaTexDelimitersStr, action = CommitText }
            ]
        }
      )
    , ( '{'
      , { name = "argBegin"
        , start = \c -> c == '{'
        , continue = \c -> False
        , endCharLength = 0 -- adjust for '}' at end of arg
        , expect =
            [ { stop = [ "}" ], action = ShiftArg }
            ]
        }
      )
    , ( '}'
      , { name = "argEnd"
        , start = \c -> c == '}'
        , continue = \c -> False
        , endCharLength = 0 -- adjust for '}' at end of arg
        , expect =
            [ { stop = [ "}" ], action = ReduceArg }
            ]
        }
      )
    ]
