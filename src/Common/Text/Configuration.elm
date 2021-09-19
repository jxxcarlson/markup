module Common.Text.Configuration exposing
    ( Configuration
    , configure
    , l1Config
    , markdownConfig
    , miniLaTeXConfig
    , miniLaTeXExpectations
    , notDelimiter
    )

{-| The actual configuration used by the parser is derived from a List Expectation
by applying the function Config.configure : List Expectation -> Configuration.

Scenario: the scanPoint field of the text cursor points to the character '['.
Because this character is in the list Configuration.beginChars, the parser
knows that it should push the expectation

    { beginSymbol = "[\n"
    , endSymbol = Just "]"
    , etype = ElementType
    , isVerbatim = False
    , markPosition = Anywhere
    }

onto the stack. If later cursor.scanPoint targets the character ']', it will
know that it should pop this element off the stack. To recall: the permissible
opreration on the cursor are

    ADD, PUSH, POP, and COMMIT

-}

import Dict exposing (Dict)
import List.Extra
import Maybe.Extra


type alias ExpectationsDict =
    Dict String Expectation


type alias Expectation =
    { beginSymbol : String
    , endSymbol : Maybe String
    , etype : ExpectationType
    , isVerbatim : Bool
    }


type ExpectationType
    = ElementType
    | CodeType
    | InlineMathType
    | QuotedType
    | FunctionName
    | VerbatimFunctionName
    | Arg


type alias Configuration =
    { beginSymbols : List String
    , endSymbols : List String
    , beginChars : List Char
    , endChars : List Char
    , delimiters : List Char
    , verbatimChars : List Char
    , expectationsDict : ExpectationsDict
    }


notDelimiter : Configuration -> Char -> Bool
notDelimiter config c =
    List.member c config.delimiters


isBeginSymbol : Configuration -> String -> Bool
isBeginSymbol config str =
    List.member str config.beginSymbols


isEndSymbol : Configuration -> String -> Bool
isEndSymbol config str =
    List.member str config.beginSymbols


l1Config =
    configure l1Expectations


markdownConfig =
    configure markdownExpectations


miniLaTeXConfig =
    configure miniLaTeXExpectations


markdownExpectations : List Expectation
markdownExpectations =
    [ { beginSymbol = "`", endSymbol = Just "`", etype = CodeType, isVerbatim = True }
    , { beginSymbol = "$", endSymbol = Just "$", etype = InlineMathType, isVerbatim = True }
    , { beginSymbol = "#", endSymbol = Nothing, etype = ElementType, isVerbatim = False }
    , { beginSymbol = "##", endSymbol = Nothing, etype = ElementType, isVerbatim = False }
    , { beginSymbol = "###", endSymbol = Nothing, etype = ElementType, isVerbatim = False }
    , { beginSymbol = "####", endSymbol = Nothing, etype = ElementType, isVerbatim = False }
    , { beginSymbol = "\"", endSymbol = Just "\"", etype = QuotedType, isVerbatim = True }
    ]


miniLaTeXExpectations : List Expectation
miniLaTeXExpectations =
    [ { beginSymbol = "\\{code}", endSymbol = Nothing, etype = VerbatimFunctionName, isVerbatim = False }
    , { beginSymbol = "\\{", endSymbol = Just "}", etype = FunctionName, isVerbatim = False }
    , { beginSymbol = "{", endSymbol = Just "}", etype = Arg, isVerbatim = False }
    , { beginSymbol = "$", endSymbol = Just "$", etype = InlineMathType, isVerbatim = True }
    , { beginSymbol = "`", endSymbol = Just "`", etype = CodeType, isVerbatim = True }
    ]


l1Expectations : List Expectation
l1Expectations =
    [ { beginSymbol = "[", endSymbol = Just "]", etype = ElementType, isVerbatim = False }
    , { beginSymbol = "`", endSymbol = Just "`", etype = CodeType, isVerbatim = True }
    , { beginSymbol = "$", endSymbol = Just "$", etype = InlineMathType, isVerbatim = True }
    , { beginSymbol = "#", endSymbol = Nothing, etype = ElementType, isVerbatim = False }
    , { beginSymbol = "##", endSymbol = Nothing, etype = ElementType, isVerbatim = False }
    , { beginSymbol = "###", endSymbol = Nothing, etype = ElementType, isVerbatim = False }
    , { beginSymbol = "####", endSymbol = Nothing, etype = ElementType, isVerbatim = False }
    , { beginSymbol = ":", endSymbol = Nothing, etype = ElementType, isVerbatim = False }
    , { beginSymbol = "\"", endSymbol = Just "\"", etype = QuotedType, isVerbatim = True }
    ]


configure : List Expectation -> Configuration
configure configDef =
    let
        beginChars =
            List.map (.beginSymbol >> firstChar) configDef |> Maybe.Extra.values

        endChars =
            List.map (.endSymbol >> Maybe.map firstChar) configDef |> Maybe.Extra.values

        verbatimChars =
            configDef |> List.filter (\d -> d.isVerbatim) |> List.map (.beginSymbol >> firstChar) |> Maybe.Extra.values
    in
    { beginSymbols = configDef |> List.map .beginSymbol |> List.Extra.unique
    , endSymbols = configDef |> List.map .endSymbol |> Maybe.Extra.values |> List.Extra.unique
    , beginChars = beginChars |> List.Extra.unique
    , endChars = endChars |> Maybe.Extra.values
    , delimiters = beginChars ++ (endChars |> Maybe.Extra.values) |> List.Extra.unique
    , verbatimChars = verbatimChars |> List.Extra.unique
    , expectationsDict = Dict.fromList (List.map (\e -> ( e.beginSymbol, e )) configDef)
    }


firstChar : String -> Maybe Char
firstChar str =
    case String.uncons str of
        Nothing ->
            Nothing

        Just ( c, _ ) ->
            Just c


name : ExpectationType -> String
name etype =
    case etype of
        ElementType ->
            "element"

        CodeType ->
            "code"

        InlineMathType ->
            "math2"

        QuotedType ->
            "quoted"

        FunctionName ->
            "functionName"

        VerbatimFunctionName ->
            "verbatimFunctionName"

        Arg ->
            "Arg"
