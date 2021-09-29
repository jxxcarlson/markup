module Common.Render.Text exposing (args, render, viewTOC)

import Common.Debug exposing (debug1)
import Common.Library.ASTTools as ASTTools
import Common.Math
import Common.Syntax as Syntax exposing (Block(..), Text(..), TextBlock(..))
import Dict exposing (Dict)
import Element exposing (Element, el, newTabLink, paragraph)
import Element.Background as Background
import Element.Font as Font
import Maybe.Extra
import MiniLaTeX.MathMacro


type alias Settings =
    { width : Int }


type alias Accumulator =
    { macroDict : MiniLaTeX.MathMacro.MathMacroDict }


render : Int -> Settings -> Accumulator -> Text -> Element msg
render generation settings accumulator text =
    case text of
        Text string meta ->
            Element.el [] (Element.text string)

        Marked name textList meta ->
            Element.el [] (renderMarked name generation settings accumulator textList)

        Verbatim name str meta ->
            renderVerbatim name generation settings accumulator str

        Arg _ _ ->
            Element.none

        TError error_ ->
            error error_


error str =
    Element.paragraph [ Background.color (Element.rgb255 250 217 215) ] [ Element.text str ]


notImplemented str =
    Element.el [ Font.color (Element.rgb255 40 40 255) ] (Element.text <| "not implemented: " ++ str)


renderVerbatim name generation settings accumulator str =
    case Dict.get name verbatimDict of
        Nothing ->
            notImplemented name

        Just f ->
            f generation settings accumulator (debug1 "XXVerbatim" str)


renderMarked name generation settings accumulator textList =
    case Dict.get name markupDict of
        Nothing ->
            notImplemented name

        Just f ->
            f generation settings accumulator textList


markupDict : Dict String (Int -> Settings -> Accumulator -> List Text -> Element msg)
markupDict =
    Dict.fromList
        [ ( "strong", \g s a textList -> strong g s a textList )
        , ( "italic", \g s a textList -> italic g s a textList )
        , ( "red", \g s a textList -> red g s a textList )
        , ( "title", \g s a textList -> Element.none )
        , ( "heading1", \g s a textList -> heading1 g s a textList )
        , ( "heading2", \g s a textList -> heading2 g s a textList )
        , ( "heading3", \g s a textList -> heading3 g s a textList )
        , ( "heading4", \g s a textList -> heading4 g s a textList )
        , ( "heading5", \g s a textList -> italic g s a textList )
        , ( "link", \g s a textList -> link g s a textList )

        -- MiniLaTeX stuff
        , ( "term", \g s a textList -> term g s a textList )
        , ( "emph", \g s a textList -> emph g s a textList )
        , ( "eqref", \g s a textList -> eqref g s a textList )
        , ( "setcounter", \g s a textList -> Element.none )
        ]


verbatimDict : Dict String (Int -> Settings -> Accumulator -> String -> Element msg)
verbatimDict =
    Dict.fromList
        [ ( "$", \g s a str -> math g s a str )
        , ( "`", \g s a str -> code g s a str )
        ]


args : List Text -> List String
args textList =
    List.map ASTTools.getText textList
        |> Maybe.Extra.values
        |> List.map String.trim
        |> List.filter (\s -> s /= "")


macro2 : (String -> String -> Element msg) -> Int -> Settings -> Accumulator -> List Text -> Element msg
macro2 element g s a textList =
    case args textList of
        -- TODO: temporary fix: parse is producing the args in reverse order
        arg1 :: arg2 :: rest ->
            element arg1 arg2

        _ ->
            el [ Font.color errorColor ] (Element.text "Invalid arguments")


link g s a textList =
    macro2 link_ g s a textList


link_ : String -> String -> Element msg
link_ label url =
    newTabLink []
        { url = url
        , label = el [ Font.color linkColor, Font.italic ] (Element.text <| label)
        }


errorColor =
    Element.rgb 0.8 0 0


linkColor =
    Element.rgb 0 0 0.8


simpleElement formatList g s a textList =
    Element.paragraph formatList (List.map (render g s a) textList)


verbatimElement formatList g s a str =
    Element.el formatList (Element.text str)


code g s a str =
    verbatimElement codeStyle g s a str


math g s a str =
    mathElement g s a str


codeStyle =
    [ Font.family
        [ Font.typeface "Inconsolata"
        , Font.monospace
        ]
    , Font.color codeColor
    , Element.paddingEach { left = 2, right = 2, top = 0, bottom = 0 }
    ]


mathElement : Int -> Settings -> Accumulator -> String -> Element msg
mathElement generation settings accumulator str =
    Common.Math.mathText generation Common.Math.InlineMathMode (MiniLaTeX.MathMacro.evalStr accumulator.macroDict str)


codeColor =
    -- E.rgb 0.2 0.5 1.0
    Element.rgb 0.4 0 0.8


tocColor =
    Element.rgb 0.1 0 0.8


viewTOC : Int -> Settings -> Accumulator -> List Syntax.Text -> List (Element msg)
viewTOC generation settings accumulator items =
    Element.el [ Font.size 18 ] (Element.text "Contents") :: List.map (viewTOCItem generation settings accumulator) items


viewTOCItem : Int -> Settings -> Accumulator -> Syntax.Text -> Element msg
viewTOCItem generation settings accumulator block =
    case block of
        Marked "heading2" textList _ ->
            paragraph (tocStyle 2) (List.map (render generation settings accumulator) textList)

        Marked "heading3" textList _ ->
            paragraph (tocStyle 3) (List.map (render generation settings accumulator) textList)

        Marked "heading4" textList _ ->
            paragraph (tocStyle 4) (List.map (render generation settings accumulator) textList)

        Marked "heading5" textList _ ->
            paragraph (tocStyle 5) (List.map (render generation settings accumulator) textList)

        _ ->
            Element.none


tocStyle k =
    [ Font.size 14, Font.color tocColor, leftPadding (k * tocPadding) ]


leftPadding k =
    Element.paddingEach { left = k, right = 0, top = 0, bottom = 0 }


tocPadding =
    8


heading1 g s a textList =
    simpleElement [ Font.size 30 ] g s a textList


heading2 g s a textList =
    simpleElement [ Font.size 22 ] g s a textList


heading3 g s a textList =
    simpleElement [ Font.size 18 ] g s a textList


heading4 g s a textList =
    simpleElement [ Font.size 14, Font.italic, Font.bold ] g s a textList


strong g s a textList =
    simpleElement [ Font.bold ] g s a textList


italic g s a textList =
    simpleElement [ Font.italic, Element.paddingEach { left = 0, right = 2, top = 0, bottom = 0 } ] g s a textList


term g s a textList =
    simpleElement [ Font.italic, Element.paddingEach { left = 0, right = 2, top = 0, bottom = 0 } ] g s a textList


eqref g s a textList =
    simpleElement [ Font.italic, Element.paddingEach { left = 0, right = 2, top = 0, bottom = 0 } ] g s a textList


emph g s a textList =
    simpleElement [ Font.italic, Element.paddingEach { left = 0, right = 2, top = 0, bottom = 0 } ] g s a textList


red g s a textList =
    simpleElement [ Font.color (Element.rgb255 200 0 0) ] g s a textList
