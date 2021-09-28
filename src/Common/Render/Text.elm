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


type alias Settings =
    { width : Int }


render : Int -> Settings -> Text -> Element msg
render generation settings text =
    case text of
        Text string meta ->
            Element.el [] (Element.text string)

        Marked name textList meta ->
            Element.el [] (renderMarked name generation settings textList)

        Verbatim name str meta ->
            renderVerbatim name generation settings str

        Arg _ _ ->
            Element.none

        TError error_ ->
            error error_


error str =
    Element.paragraph [ Background.color (Element.rgb255 250 217 215) ] [ Element.text str ]


notImplemented str =
    Element.el [ Font.color (Element.rgb255 40 40 255) ] (Element.text <| "not implemented: " ++ str)


renderVerbatim name generation settings str =
    case Dict.get name verbatimDict of
        Nothing ->
            notImplemented name

        Just f ->
            f generation settings (debug1 "XXVerbatim" str)


renderMarked name generation settings textList =
    case Dict.get name markupDict of
        Nothing ->
            notImplemented name

        Just f ->
            f generation settings textList


markupDict : Dict String (Int -> Settings -> List Text -> Element msg)
markupDict =
    Dict.fromList
        [ ( "strong", \g s textList -> strong g s textList )
        , ( "italic", \g s textList -> italic g s textList )
        , ( "red", \g s textList -> red g s textList )
        , ( "title", \g s textList -> Element.none )
        , ( "heading1", \g s textList -> heading1 g s textList )
        , ( "heading2", \g s textList -> heading2 g s textList )
        , ( "heading3", \g s textList -> heading3 g s textList )
        , ( "heading4", \g s textList -> heading4 g s textList )
        , ( "heading5", \g s textList -> italic g s textList )
        , ( "link", \g s textList -> link g s textList )
        ]


verbatimDict : Dict String (Int -> Settings -> String -> Element msg)
verbatimDict =
    Dict.fromList
        [ ( "$", \g s str -> math g s str )
        , ( "`", \g s str -> code g s str )
        ]


args : List Text -> List String
args textList =
    List.map ASTTools.getText textList
        |> Maybe.Extra.values
        |> List.map String.trim
        |> List.filter (\s -> s /= "")


link g s textList =
    case args textList of
        -- TODO: temporary fix: parse is producing the args in reverse order
        label :: url :: rest ->
            newTabLink []
                { url = url
                , label = el [ Font.color linkColor, Font.italic ] (Element.text <| label)
                }

        _ ->
            el [ Font.color errorColor ] (Element.text "Invalid link")


errorColor =
    Element.rgb 0.8 0 0


linkColor =
    Element.rgb 0 0 0.8


simpleElement formatList g s textList =
    Element.paragraph formatList (List.map (render g s) textList)


verbatimElement formatList g s str =
    Element.el formatList (Element.text str)


code g s str =
    verbatimElement codeStyle g s str


math g s str =
    mathElement g s str


codeStyle =
    [ Font.family
        [ Font.typeface "Inconsolata"
        , Font.monospace
        ]
    , Font.color codeColor
    , Element.paddingEach { left = 2, right = 2, top = 0, bottom = 0 }
    ]


mathElement : Int -> Settings -> String -> Element msg
mathElement generation settings str =
    Common.Math.mathText generation Common.Math.InlineMathMode str


codeColor =
    -- E.rgb 0.2 0.5 1.0
    Element.rgb 0.4 0 0.8


tocColor =
    Element.rgb 0.1 0 0.8


viewTOC : Int -> Settings -> List Syntax.Text -> List (Element msg)
viewTOC generation settings items =
    Element.el [ Font.size 18 ] (Element.text "Contents") :: List.map (viewTOCItem generation settings) items


viewTOCItem : Int -> Settings -> Syntax.Text -> Element msg
viewTOCItem generation settings block =
    case block of
        Marked "heading2" textList _ ->
            paragraph (tocStyle 2) (List.map (render generation settings) textList)

        Marked "heading3" textList _ ->
            paragraph (tocStyle 3) (List.map (render generation settings) textList)

        Marked "heading4" textList _ ->
            paragraph (tocStyle 4) (List.map (render generation settings) textList)

        Marked "heading5" textList _ ->
            paragraph (tocStyle 5) (List.map (render generation settings) textList)

        _ ->
            Element.none


tocStyle k =
    [ Font.size 14, Font.color tocColor, leftPadding (k * tocPadding) ]


leftPadding k =
    Element.paddingEach { left = k, right = 0, top = 0, bottom = 0 }


tocPadding =
    8


heading1 g s textList =
    -- simpleElement [ Font.size 32, Font.color (Element.rgb255 200 0 0) ] g s textList
    --Element.paragraph [ Font.size 32 ] (List.map (render g s) textList)
    simpleElement [ Font.size 30 ] g s textList


heading2 g s textList =
    simpleElement [ Font.size 22 ] g s textList


heading3 g s textList =
    simpleElement [ Font.size 18 ] g s textList


heading4 g s textList =
    simpleElement [ Font.size 14, Font.italic, Font.bold ] g s textList


strong : Int -> Settings -> List Text -> Element msg
strong g s textList =
    Element.paragraph [ Font.bold ] (List.map (render g s) textList)


italic : Int -> Settings -> List Text -> Element msg
italic g s textList =
    Element.paragraph [ Font.italic, Element.paddingEach { left = 0, right = 2, top = 0, bottom = 0 } ] (List.map (render g s) textList)


red : Int -> Settings -> List Text -> Element msg
red g s textList =
    Element.paragraph [ Font.color (Element.rgb255 200 0 0) ] (List.map (render g s) textList)
