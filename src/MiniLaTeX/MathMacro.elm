module MiniLaTeX.MathMacro exposing (MathMacroDict, evalList, evalMacro, evalStr, makeMacroDict, newCommand2, parse, parseMany)

import Dict exposing (Dict)
import List.Extra
import Parser.Advanced exposing (..)
import Result.Extra
import Set


{-| The type for the syntax tree
-}



--InlineMath str ->
--    Html.span [] [ oneSpace, inlineMathText latexState (Internal.MathMacro.evalStr latexState.mathMacroDictionary str) ]
--
--DisplayMath str ->
--    -- TODO: fix Internal.MathMacro.evalStr.  It is nuking \begin{pmacro}, etc.
--    displayMathText latexState (Internal.MathMacro.evalStr latexState.mathMacroDictionary str)
-- s1 = "\\newcommand{\\bb}[0]{\\bf{B}}
--envReducer : String -> List LatexExpression -> LatexExpression -> LatexState -> LatexState
--envReducer name optonalArgs body state =
--    if List.member name theoremWords then
--        ReducerHelper.setTheoremNumber body state
--
--    else
--        case name of
--            "equation" ->
--                ReducerHelper.setEquationNumber body state
--
--            "align" ->
--                ReducerHelper.setEquationNumber body state
--
--            "mathmacro" ->
--                case body of
--                    LXString str ->
--                        let
--                            mathDict =
--                                Internal.MathMacro.makeMacroDict (String.trim str)
--                        in
--                        { state | mathMacroDictionary = mathDict }
--
--                    _ ->
--                        state
--
--            "textmacro" ->
--                case body of
--                    LXString str ->
--                        ReducerHelper.setDictionary str state
--
--                    _ ->
--                        state
--
--            _ ->
--                state
--


type MathExpression
    = MathText String
    | Macro MacroName (List MathExpression)
    | NewCommand MacroName NumberOfArguments (List MathExpression)
    | MathList (List MathExpression)


type alias MacroName =
    String


type alias NumberOfArguments =
    String


type MacroBody
    = MacroBody Int (List MathExpression)


type alias MathMacroDict =
    Dict String MacroBody


makeMacroDict : String -> MathMacroDict
makeMacroDict str =
    case parse str of
        Ok list ->
            list
                |> List.map makeEntry
                |> Dict.fromList

        Err _ ->
            Dict.empty


makeEntry : MathExpression -> ( String, MacroBody )
makeEntry mathExpression_ =
    case mathExpression_ of
        NewCommand macroName_ nArgs body ->
            ( macroName_, MacroBody (String.toInt nArgs |> Maybe.withDefault 0) body )

        _ ->
            ( "nullMacro", MacroBody 0 [ MathText "0" ] )



-- EVAL


{-|

    > s1 = "\\newcommand{\\bb}[0]{\\bf{B}}"
    "\\newcommand{\\bb}[0]{\\bf{B}}" : String
    > d = makeMacroDict s1
    Dict.fromList [("bb",MacroBody 0 [MathList [Macro "bf" [MathList [MathText "B"]]]])]

    > evalStr d "This is $\\alpha = \\bb^3$"
    "This is $ \\alpha =  \\bf{B} ^3$




    OLD:

    d2 = makeMacroDict "\\newcommand{\\bb}[0]{\\bf{B}}\n\\newcommand{\\bt}[1]{\\bf{#1}}"
    --> Dict.fromList [("bb",<function>),("bt",<function>)]

    evalStr d2 "\\int_0^1 x^n dx + \\bb + \\bt{R}"
    --> "\\int_0^1 x^n dx + \\bf{B}+ \\bf{R}"

-}
evalStr : MathMacroDict -> String -> String
evalStr macroDict_ str =
    case parseMany (String.trim str) of
        Ok result ->
            evalList macroDict_ result

        Err _ ->
            str


evalList : MathMacroDict -> List MathExpression -> String
evalList macroDict_ list =
    List.map (evalMathExpr macroDict_) list
        |> String.join " "


evalMathExpr : MathMacroDict -> MathExpression -> String
evalMathExpr macroDict_ expr =
    case expr of
        MathText str ->
            str

        Macro name args ->
            evalMacro macroDict_ name args

        NewCommand name nargs args ->
            evalNewCommand name nargs args

        MathList list ->
            evalList macroDict_ list


{-|

    > d = makeMacroDict  s1
    > ebb = evalMacro d "bb"
    > Result.map ebb (parse "x == \\bb^3")
    --> Ok "\\bf{B}"

-}
evalMacro : MathMacroDict -> String -> List MathExpression -> String
evalMacro macroDict_ name args =
    case Dict.get name macroDict_ of
        Nothing ->
            "\\" ++ name ++ (List.map (evalMathExpr macroDict_ >> enclose) args |> String.join "")

        Just (MacroBody n body) ->
            transform n body (List.map (evalMathExpr macroDict_) args)


transform : Int -> List MathExpression -> List String -> String
transform n args =
    List.map toText_ args
        |> List.head
        |> Maybe.withDefault "XXX"
        |> (\str -> \list -> str)
        |> replaceArgs n


replaceArgs : Int -> (List String -> String) -> (List String -> String)
replaceArgs n f =
    List.foldl replaceArg f (List.range 0 (n - 1))


{-|

    f0 = \list -> "\\bf{#1}"
    --> <function> : a -> String

    f1 = replaceArg 0 f0
    --> <function> : List String -> String

    f1 ["a"]
    --> "\\bf{a}"

-}
replaceArg : Int -> (List String -> String) -> (List String -> String)
replaceArg k f =
    \list -> String.replace ("#" ++ String.fromInt (k + 1)) (getArg k list) (f list)



-- [NewCommand "bt" "1" [MathList [MathText "{",Macro "bf" [],MathText "#1"]],MathText "}"]


getArg : Int -> List String -> String
getArg k list =
    List.Extra.getAt k list |> Maybe.withDefault ""



-- PARSER


{-|

    parse "x + y = \\foo{bar} + z"
    --> Ok [MathText ("x + y = "),Macro "foo" [MathList [MathText "bar"]],MathText ("+ z")]

-}
parse : String -> Result (List (DeadEnd Context Problem)) (List MathExpression)
parse str =
    run (many mathExpression) str


type alias MXParser a =
    Parser.Advanced.Parser Context Problem a


type Context
    = CArg String
    | List


type Problem
    = ExpectingLeftBrace
    | ExpectingRightBrace
    | ExpectingMacroReservedWord
    | ExpectingValidMacroArgWord
    | ExpectingLeftBracket
    | ExpectingInt
    | ExpectingRightBracket
    | InvalidInt
    | ExpectingStuff
    | ExpectingNewCommand
    | ExpectingBackslash


parseMany : String -> Result (List (DeadEnd Context Problem)) (List MathExpression)
parseMany str =
    str
        |> String.trim
        |> String.lines
        |> List.map String.trim
        |> List.map parse
        |> Result.Extra.combine
        |> Result.map List.concat


check : String -> String
check str =
    case parse str of
        Ok result ->
            toText result

        Err _ ->
            "error"



-- makeEntry_  : String -> String -> List MathExpression -> (String, List String -> String)
-- MAP PARSE EXPR TO TEXT


toText : List MathExpression -> String
toText list =
    List.map toText_ list
        |> String.join ""


toText_ : MathExpression -> String
toText_ expr =
    case expr of
        MathText str ->
            str

        Macro name args ->
            "\\" ++ name ++ (List.map (toText_ >> enclose) args |> String.join "")

        MathList list ->
            List.map toText_ list |> String.join " "

        NewCommand name nargs args ->
            evalNewCommand name nargs args


evalNewCommand : String -> String -> List MathExpression -> String
evalNewCommand name nargs args =
    "\\newcommand{\\" ++ name ++ "}[" ++ nargs ++ "]" ++ (List.map (toText_ >> enclose) args |> String.join "")


enclose : String -> String
enclose arg_ =
    "{" ++ arg_ ++ "}"



-- PARSER


mathExpression : MXParser MathExpression
mathExpression =
    oneOf
        [ backtrackable newCommand
        , macro
        , mathStuff
        ]


{-|

    run mathStuff "x + y = \\foo"
    --> Ok (MathText ("x + y = "))

-}
mathStuff : MXParser MathExpression
mathStuff =
    stuff ExpectingStuff inStuff |> map MathText


inStuff : Char -> Bool
inStuff c =
    not (c == '\\')


{-|

    run (stuff ExpectingStuff inStuff) "x + y = \\foo"
    --> Ok ("x + y = ")

-}
stuff : Problem -> (Char -> Bool) -> MXParser String
stuff problem inWord =
    succeed String.slice
        |. ws
        |= getOffset
        |. chompIf inStuff problem
        |. chompWhile inStuff
        |. ws
        |= getOffset
        |= getSource


{-|

    import MXParser

    MXParser.run numberOfArgs "[3]"
    --> Ok 3

-}
numberOfArgs : MXParser Int
numberOfArgs =
    many numberOfArgs_
        |> map List.head
        |> map (Maybe.withDefault 0)


{-| Parse the macro keyword followed by
zero or more more arguments.

    run (macro ws) "\\bf{1}"
    --> Ok (Macro "bf" [MathList [MathText "1"]])

    run (macro ws) "\\foo"
    --> Ok (Macro "foo" [])

-}
macro : MXParser MathExpression
macro =
    succeed Macro
        |= macroName
        -- |= itemList (oneOf [ lazy (\_ -> many mathExpression |> map MathList), arg ])
        |= itemList arg
        |. ws



-- |. wsParser


newCommand =
    oneOf [ backtrackable newCommand1, newCommand2 ]


newCommand1 =
    succeed NewCommand
        |. symbol (Token "\\newcommand" ExpectingNewCommand)
        |= newMacroName
        |. symbol (Token "[" ExpectingLeftBracket)
        |= word ExpectingRightBracket (\c -> c /= ']')
        |. symbol (Token "]" ExpectingRightBracket)
        |= itemList arg
        |. ws


newCommand2 =
    succeed (\x y -> NewCommand x "0" y)
        |. symbol (Token "\\newcommand" ExpectingNewCommand)
        |= newMacroName
        |= itemList arg
        |. ws


{-| Use to parse arguments for macros
-}
arg : MXParser MathExpression
arg =
    inContext (CArg "arg") <|
        (succeed identity
            |. symbol (Token "{" ExpectingLeftBrace)
            |. ws
            -- |= itemList (oneOf [ macroArgWords, lazy (\_ -> macro) ])
            |= argList
            |. symbol (Token "}" ExpectingRightBrace)
            |> map MathList
        )


argList : MXParser (List MathExpression)
argList =
    itemList (oneOf [ macroArgWords, lazy (\_ -> macro) ])



-- itemList (oneOf [ mathStuff, lazy (\_ -> macro) ])


newMacroName : MXParser String
newMacroName =
    inContext (CArg "arg") <|
        (succeed identity
            |. symbol (Token "{" ExpectingLeftBrace)
            |. symbol (Token "\\" ExpectingBackslash)
            |= word ExpectingRightBrace (\c -> c /= '}')
            |. symbol (Token "}" ExpectingRightBrace)
        )


macroName : MXParser String
macroName =
    variable
        { start = \c -> c == '\\'
        , inner = \c -> Char.isAlphaNum c || c == '*'
        , reserved = Set.fromList [ "\\item", "\\bibitem" ]
        , expecting = ExpectingMacroReservedWord
        }
        |> map (String.dropLeft 1)



{- MACRO WORDS -}


macroArgWords : MXParser MathExpression
macroArgWords =
    nonEmptyItemList (word ExpectingValidMacroArgWord inMacroArg)
        |> map (String.join " ")
        |> map MathText


inMacroArg : Char -> Bool
inMacroArg c =
    not (c == '\\' || c == '$' || c == '}' || c == ' ' || c == '\n')


{-| Use `inWord` to parse a word.

import Parser

inWord : Char -> Bool
inWord c = not (c == ' ')

MXParser.run word "this is a test"
--> Ok "this"

-}
word : Problem -> (Char -> Bool) -> MXParser String
word problem inWord =
    succeed String.slice
        |. ws
        |= getOffset
        |. chompIf inWord problem
        |. chompWhile inWord
        |. ws
        |= getOffset
        |= getSource


numberOfArgs_ : MXParser Int
numberOfArgs_ =
    succeed identity
        |. symbol (Token "[" ExpectingLeftBracket)
        |= int ExpectingInt InvalidInt
        |. symbol (Token "]" ExpectingRightBracket)



-- HELPERS


spaces : MXParser ()
spaces =
    chompWhile (\c -> c == ' ')


ws : MXParser ()
ws =
    chompWhile (\c -> c == ' ' || c == '\n')


parseUntil : Problem -> String -> MXParser String
parseUntil problem marker =
    getChompedString <| chompUntil (Token marker problem)


{-| chomp to end of the marker and return the
chomped string minus the marker.
-}
parseToSymbol : Problem -> String -> MXParser String
parseToSymbol problem marker =
    (getChompedString <|
        succeed identity
            |= chompUntilEndOr marker
            |. symbol (Token marker problem)
    )
        |> map (String.dropRight (String.length marker))


parseBetweenSymbols : Problem -> Problem -> String -> String -> MXParser String
parseBetweenSymbols problem1 problem2 startSymbol endSymbol =
    succeed identity
        |. symbol (Token startSymbol problem1)
        |. spaces
        |= parseUntil problem2 endSymbol


nonEmptyItemList : MXParser a -> MXParser (List a)
nonEmptyItemList itemParser =
    itemParser
        |> andThen (\x -> itemList_ [ x ] itemParser)


itemList : MXParser a -> MXParser (List a)
itemList itemParser =
    itemList_ [] itemParser


itemList_ : List a -> MXParser a -> MXParser (List a)
itemList_ initialList itemParser =
    loop initialList (itemListHelper itemParser)


itemListHelper : MXParser a -> List a -> MXParser (Step (List a) (List a))
itemListHelper itemParser revItems =
    oneOf
        [ succeed (\item_ -> Loop (item_ :: revItems))
            |= itemParser
        , succeed ()
            |> map (\_ -> Done (List.reverse revItems))
        ]


{-| Apply a parser zero or more times and return a list of the results.
-}
many : MXParser a -> MXParser (List a)
many p =
    loop [] (manyHelp p)


manyHelp : MXParser a -> List a -> MXParser (Step (List a) (List a))
manyHelp p vs =
    oneOf
        [ succeed (\v -> Loop (v :: vs))
            |= p
            |. spaces
        , succeed ()
            |> map (\_ -> Done (List.reverse vs))
        ]


{-| Apply a parser one or more times and return a tuple of the first result parsed
and the list of the remaining results.
-}
some : MXParser a -> MXParser ( a, List a )
some p =
    succeed Tuple.pair
        |= p
        |. spaces
        |= many p


{-| Parse an expression between two other parser

    import Parser exposing(symbol)

    Parser.run (between (symbol "<<") (symbol ">>") Parser.int) "<<4>>"
    --> Ok 4

-}
between : MXParser opening -> MXParser closing -> MXParser a -> MXParser a
between opening closing p =
    succeed identity
        |. opening
        |. spaces
        |= p
        |. spaces
        |. closing
