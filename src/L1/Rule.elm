module L1.Rule exposing (rules)

import Common.Text.Rule exposing (Action(..), Rule, Rules)
import Dict exposing (Dict)


rules : Rules
rules =
    { dict = l1RuleDict, default = defaultRule }


l1RuleDict : Dict Char Rule
l1RuleDict =
    Dict.fromList l1RuleList


defaultRule : Rule
defaultRule =
    { name = "alpha"
    , start = \c -> not (List.member c (' ' :: l1Delimiters))
    , continue = \c -> not (List.member c l1Delimiters)
    , spaceFollows = False
    , endCharLength = 0
    , dropLeadingChars = 1
    , isVerbatim = False
    , transform = identity
    , expect = [ { stop = l1DelimiterStr, action = ShiftText } ]
    }


l1Delimiters =
    [ '`', '$', '[', ']' ]


l1DelimiterStr =
    [ "`", "$", "[", "]" ]


transformHeading : String -> String
transformHeading str =
    case str of
        "#" ->
            "title"

        "##" ->
            "heading2"

        "###" ->
            "heading3"

        "####" ->
            "heading4"

        "#####" ->
            "heading5"

        _ ->
            str


transformMarked : String -> String
transformMarked str =
    case str of
        "i" ->
            "italic"

        "b" ->
            "strong"

        "h1" ->
            "heading1"

        "h2" ->
            "heading2"

        "h3" ->
            "heading3"

        "h4" ->
            "heading4"

        "h5" ->
            "heading5"

        _ ->
            str


l1RuleList =
    [ ( '#'
      , { name = "title"
        , start = \c -> c == '#'
        , continue = \c -> c /= ' '
        , spaceFollows = True
        , endCharLength = 0
        , dropLeadingChars = 0
        , isVerbatim = False
        , transform = transformHeading
        , expect =
            [ { stop = [ " " ], action = ShiftMarked }
            ]
        }
      )
    , ( '['
      , { name = "annotationBegin"
        , start = \c -> c == '['
        , continue = \c -> c /= ' '
        , spaceFollows = True
        , endCharLength = 0
        , dropLeadingChars = 1
        , isVerbatim = False
        , transform = transformMarked
        , expect =
            [ { stop = [ "]" ], action = ShiftMarked }
            ]
        }
      )
    , ( ']'
      , { name = "annotationEnd"
        , start = \c -> c == ']'
        , continue = \c -> False
        , spaceFollows = False
        , endCharLength = 0
        , dropLeadingChars = 0
        , isVerbatim = False
        , transform = identity
        , expect =
            [ { stop = [ "]" ], action = ReduceArg }
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
        , transform = identity
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
        , transform = identity
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
        , transform = identity
        , expect =
            [ { stop = l1DelimiterStr, action = Commit }
            ]
        }
      )
    ]
