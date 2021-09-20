module Markdown.Rule exposing (markdownRules)

import Common.Text.Rule exposing (Action(..), Rule, Rules)
import Dict exposing (Dict)


markdownRules : Rules
markdownRules =
    { dict = markdownRuleDict, default = defaultRule }


markdownRuleDict : Dict Char Rule
markdownRuleDict =
    Dict.fromList markdownRuleList


defaultRule : Rule
defaultRule =
    { name = "alpha"
    , start = \c -> not (List.member c (' ' :: markdownDelimiters))
    , continue = \c -> not (List.member c markdownDelimiters)
    , endCharLength = 0
    , dropLeadingChars = 1
    , expect = [ { stop = markdownDelimitersStr, action = ShiftText } ]
    }


markdownDelimiters =
    [ '*', '_', '`', '$', '[', ']', '#' ]


markdownDelimitersStr =
    [ "*", "_", "`", "$", "[", "]", "#" ]


markdownRuleList =
    [ ( '#'
      , { name = "title"
        , start = \c -> c == '#'
        , continue = \c -> c /= ' '
        , endCharLength = 0
        , dropLeadingChars = 0
        , expect =
            [ { stop = [ " " ], action = ShiftMarked }
            ]
        }
      )
    , ( '*'
      , { name = "bold"
        , start = \c -> c == '*'
        , continue = \c -> False
        , endCharLength = 0
        , dropLeadingChars = 0
        , expect =
            [ { stop = [ "*" ], action = ShiftMarked }
            ]
        }
      )
    , ( '_'
      , { name = "italic"
        , start = \c -> c == '_'
        , continue = \c -> False
        , endCharLength = 0
        , dropLeadingChars = 0
        , expect =
            [ { stop = [ "_" ], action = ShiftMarked }
            ]
        }
      )
    , ( ' '
      , { name = "blank"
        , start = \c -> c == ' '
        , continue = \c -> not (List.member c markdownDelimiters)
        , endCharLength = 0
        , dropLeadingChars = 1
        , expect =
            [ { stop = markdownDelimitersStr, action = CommitText }
            ]
        }
      )
    ]
