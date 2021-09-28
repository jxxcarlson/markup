module MiniLaTeX.Rule exposing (rules)

import Common.Text.Rule exposing (Action(..), ParseEnd(..), Rule, Rules)
import Dict exposing (Dict)


transformMacros : String -> String
transformMacros str =
    case str of
        "section" ->
            "heading2"

        "subsection" ->
            "heading3"

        "susubsection" ->
            "heading4"

        "subheading" ->
            "heading5"

        _ ->
            str


rules : Rules
rules =
    { dict = miniLaTeXRuleDict, default = defaultRule }


miniLaTeXRuleDict : Dict Char Rule
miniLaTeXRuleDict =
    Dict.fromList miniLaTeXRuleList


defaultRule : Rule
defaultRule =
    { name = "alpha"
    , start = \c -> not (List.member c (' ' :: miniLaTexDelimiters))
    , continue = \c -> not (List.member c miniLaTexDelimiters)
    , parseEnd = EndNormal
    , endCharLength = 0
    , dropLeadingChars = 1
    , isVerbatim = False
    , transform = identity
    , expect = [ { stop = miniLaTexDelimitersStr, action = ShiftText } ]
    }


miniLaTexDelimiters =
    [ '\\', '{', '}', '$' ]


miniLaTexDelimitersStr =
    [ "\\", "{", "}", "$" ]


miniLaTeXRuleList =
    [ ( '\\'
      , { name = "macro"
        , start = \c -> c == '\\'
        , continue = \c -> not (c == ' ' || c == '{')
        , parseEnd = EndNormal
        , endCharLength = 0
        , dropLeadingChars = 1
        , isVerbatim = False
        , transform = transformMacros
        , expect =
            [ { stop = [ " ", "" ], action = CommitMarked }
            , { stop = [ "{" ], action = ShiftMarked }
            ]
        }
      )
    , ( ' '
      , { name = "blank"
        , start = \c -> c == ' '
        , continue = \c -> c == ' '
        , parseEnd = EndNormal
        , endCharLength = 0
        , dropLeadingChars = 1
        , isVerbatim = False
        , transform = identity
        , expect =
            [ { stop = miniLaTexDelimitersStr, action = Commit }
            ]
        }
      )
    , ( '{'
      , { name = "argBegin"
        , start = \c -> c == '{'
        , continue = \c -> False
        , parseEnd = EndNormal
        , endCharLength = 0 -- adjust for '}' at end of arg
        , dropLeadingChars = 1
        , isVerbatim = False
        , transform = identity
        , expect =
            [ { stop = [ "}" ], action = ShiftArg }
            ]
        }
      )
    , ( '}'
      , { name = "argEnd"
        , start = \c -> c == '}'
        , continue = \c -> False
        , parseEnd = EndNormal
        , endCharLength = 0 -- adjust for '}' at end of arg
        , dropLeadingChars = 1
        , isVerbatim = False
        , transform = identity
        , expect =
            [ { stop = [ "}" ], action = ReduceArg }
            ]
        }
      )
    , ( '$'
      , { name = "math"
        , start = \c -> c == '$'
        , continue = \c -> False
        , parseEnd = EndNormal
        , endCharLength = 0
        , dropLeadingChars = 0
        , isVerbatim = True
        , transform = identity
        , expect =
            [ { stop = [ "$" ], action = ShiftVerbatim "$" }
            ]
        }
      )
    ]
