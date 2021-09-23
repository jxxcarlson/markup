module Markdown.Rule exposing (rules)

import Common.Text.Rule exposing (Action(..), Rule, Rules)
import Dict exposing (Dict)


rules : Rules
rules =
    { dict = markdownRuleDict, default = defaultRule }


markdownRuleDict : Dict Char Rule
markdownRuleDict =
    Dict.fromList markdownRuleList


defaultRule : Rule
defaultRule =
    { name = "alpha"
    , start = \c -> not (List.member c (' ' :: markdownDelimiters))
    , continue = \c -> not (List.member c markdownDelimiters)
    , spaceFollows = False
    , endCharLength = 0
    , dropLeadingChars = 1
    , isVerbatim = False
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
        , spaceFollows = True
        , endCharLength = 0
        , dropLeadingChars = 0
        , isVerbatim = False
        , expect =
            [ { stop = [ " " ], action = ShiftMarked }
            ]
        }
      )
    , ( '*'
      , { name = "bold"
        , start = \c -> c == '*'
        , continue = \c -> False
        , spaceFollows = False
        , endCharLength = 0
        , dropLeadingChars = 0
        , isVerbatim = False
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
        , spaceFollows = True
        , isVerbatim = False
        , expect =
            [ { stop = [ "_" ], action = ShiftMarked }
            ]
        }
      )
    , ( '`'
      , { name = "code"
        , start = \c -> c == '`'
        , continue = \c -> False
        , spaceFollows = False
        , endCharLength = 0
        , dropLeadingChars = 0
        , isVerbatim = True
        , expect =
            [ { stop = [ "`" ], action = ShiftVerbatim "`" }
            ]
        }
      )
    , ( '$'
      , { name = "math"
        , start = \c -> c == '$'
        , continue = \c -> False
        , spaceFollows = False
        , endCharLength = 0
        , dropLeadingChars = 0
        , isVerbatim = True
        , expect =
            [ { stop = [ "$" ], action = ShiftVerbatim "$" }
            ]
        }
      )
    , ( ' '
      , { name = "blank"
        , start = \c -> c == ' '
        , continue = \c -> c == ' '
        , spaceFollows = False
        , endCharLength = 0
        , dropLeadingChars = 1
        , isVerbatim = False
        , expect =
            [ { stop = markdownDelimitersStr, action = Commit }
            ]
        }
      )
    ]
