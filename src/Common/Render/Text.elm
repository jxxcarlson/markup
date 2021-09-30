module Common.Render.Text exposing (args, render, viewTOC)

import Common.Debug exposing (debug1)
import Common.Library.ASTTools as ASTTools
import Common.Math
import Common.Syntax as Syntax exposing (Block(..), Text(..), TextBlock(..))
import Common.Text
import Dict exposing (Dict)
import Element exposing (Element, alignLeft, alignRight, centerX, column, el, newTabLink, paddingEach, paragraph, px, spacing)
import Element.Background as Background
import Element.Font as Font
import Maybe.Extra
import MiniLaTeX.MathMacro
import Utility


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
        , ( "href", \g s a textList -> href g s a textList )
        , ( "image", \g s a textList -> image g s a textList )

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


href g s a textList =
    macro2 href_ g s a textList


href_ : String -> String -> Element msg
href_ url label =
    newTabLink []
        { url = url
        , label = el [ Font.color linkColor, Font.italic ] (Element.text <| label)
        }



--         , ( "href", \g s a textList -> href g s a textList )


image generation settings accumuator body =
    let
        arguments =
            args body

        url =
            List.head arguments |> Maybe.withDefault "no-image"

        dict =
            Utility.keyValueDict (List.drop 1 arguments)

        description =
            Dict.get "caption" dict |> Maybe.withDefault ""

        caption =
            case Dict.get "caption" dict of
                Nothing ->
                    Element.none

                Just c ->
                    Element.row [ placement, Element.width Element.fill ] [ el [ Element.width Element.fill ] (Element.text c) ]

        width =
            case Dict.get "width" dict of
                Nothing ->
                    px displayWidth

                Just w_ ->
                    case String.toInt w_ of
                        Nothing ->
                            px displayWidth

                        Just w ->
                            px w

        placement =
            case Dict.get "placement" dict of
                Nothing ->
                    centerX

                Just "left" ->
                    alignLeft

                Just "right" ->
                    alignRight

                Just "center" ->
                    centerX

                _ ->
                    centerX

        displayWidth =
            settings.width
    in
    column [ spacing 8, Element.width (px displayWidth), placement ]
        [ Element.image [ Element.width width, placement ]
            { src = url, description = description }
        , caption
        ]


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



--
--tocItem : Element -> E.Element msg
--tocItem e =
--    case AST.getTextList2 e of
--        n :: content :: rest ->
--            el [ paddingEach { left = tocPadding n, right = 0, top = 0, bottom = 0 }, Font.color (E.rgb255 46 33 194) ]
--                (E.link [] { url = internalLink content, label = text (AST.getLabel e ++ ". " ++ content) })
--
--        _ ->
--            E.none


internalLink : String -> String
internalLink str =
    "#" ++ str |> makeSlug


tocLink : List Text -> Element msg
tocLink textList =
    let
        t =
            Common.Text.stringValueOfList textList
    in
    Element.link [] { url = internalLink t, label = Element.text t }


viewTOCItem : Int -> Settings -> Accumulator -> Syntax.Text -> Element msg
viewTOCItem generation settings accumulator block =
    case block of
        Marked "heading2" textList _ ->
            el (tocStyle 2) (tocLink textList)

        Marked "heading3" textList _ ->
            el (tocStyle 3) (tocLink textList)

        Marked "heading4" textList _ ->
            el (tocStyle 4) (tocLink textList)

        Marked "heading5" textList _ ->
            el (tocStyle 5) (tocLink textList)

        _ ->
            Element.none


tocStyle k =
    [ Font.size 14, Font.color tocColor, leftPadding (k * tocPadding) ]


leftPadding k =
    Element.paddingEach { left = k, right = 0, top = 0, bottom = 0 }


tocPadding =
    8


makeSlug : String -> String
makeSlug str =
    str |> String.toLower |> String.replace " " "-"


makeId : List Text -> Element.Attribute msg
makeId textList =
    Utility.elementAttribute "id" (Common.Text.stringValueOfList textList |> makeSlug)


heading1 g s a textList =
    simpleElement [ Font.size 30, makeId textList ] g s a textList


heading2 g s a textList =
    simpleElement [ Font.size 22, makeId textList ] g s a textList


heading3 g s a textList =
    simpleElement [ Font.size 18, makeId textList ] g s a textList


heading4 g s a textList =
    simpleElement [ Font.size 14, Font.italic, Font.bold, makeId textList ] g s a textList


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
