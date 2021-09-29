module Common.Render.TextBlock exposing (Settings, render)

import Common.Debug exposing (debug3)
import Common.Library.ASTTools as ASTTools
import Common.Math
import Common.Render.Text
import Common.Syntax as Syntax exposing (Block(..), Text(..), TextBlock(..))
import Common.TextBlock
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font


type alias Settings =
    { width : Int }


render : Int -> Settings -> List TextBlock -> List (Element msg)
render generation settings blocks =
    List.map (renderBlock generation settings) blocks


renderBlock : Int -> Settings -> TextBlock -> Element msg
renderBlock generation settings block =
    case block of
        TBParagraph textList _ ->
            paragraph
                []
                (List.map (Common.Render.Text.render generation settings) textList)

        TBVerbatimBlock name lines _ ->
            case Dict.get name verbatimBlockDict of
                Nothing ->
                    error ("Unimplemented verbatim block: " ++ name)

                Just f ->
                    f generation settings lines

        TBBlock name blocks _ ->
            case Dict.get name blockDict of
                Nothing ->
                    error ("Unimplemented block: " ++ name)

                Just f ->
                    f generation settings blocks

        TBError desc ->
            error desc


error str =
    paragraph [ Background.color (rgb255 250 217 215) ] [ text str ]


verbatimBlockDict : Dict String (Int -> Settings -> List String -> Element msg)
verbatimBlockDict =
    Dict.fromList
        [ ( "code", \g s lines -> codeBlock g s lines )
        , ( "math", \g s lines -> mathBlock g s lines )
        , ( "equation", \g s lines -> equation g s lines )
        , ( "align", \g s lines -> aligned g s lines )
        , ( "mathmacro", \g s lines -> Element.none )
        ]


blockDict : Dict String (Int -> Settings -> List TextBlock -> Element msg)
blockDict =
    Dict.fromList
        [ ( "quotation", \g s blocks -> quotationBlock g s blocks )
        , ( "item", \g s blocks -> item g s blocks )
        ]


codeBlock : Int -> Settings -> List String -> Element msg
codeBlock generation settings textList =
    column
        [ Font.family
            [ Font.typeface "Inconsolata"
            , Font.monospace
            ]
        , Font.color codeColor
        , paddingEach { left = 0, right = 0, top = 0, bottom = 8 }
        , spacing 6
        ]
        (List.map (\t -> el [] (text t)) (List.map (String.dropLeft 0) textList))


mathBlock : Int -> Settings -> List String -> Element msg
mathBlock generation settings textList =
    Common.Math.mathText generation Common.Math.DisplayMathMode (String.join "\n" textList)


equation : Int -> Settings -> List String -> Element msg
equation generation settings textList =
    Common.Math.mathText generation Common.Math.DisplayMathMode (String.join "\n" textList)


aligned : Int -> Settings -> List String -> Element msg
aligned generation settings textList =
    Common.Math.mathText generation Common.Math.DisplayMathMode ("\\begin{aligned}\n" ++ String.join "\n" textList ++ "\n\\end{aligned}")


quotationBlock : Int -> Settings -> List Syntax.TextBlock -> Element msg
quotationBlock generation settings blocks =
    column
        [ paddingEach { left = 18, right = 0, top = 0, bottom = 8 }
        ]
        (List.map (renderBlock generation settings) (debug3 "XX, block in quotation" blocks))


item : Int -> Settings -> List Syntax.TextBlock -> Element msg
item generation settings blocks =
    row [ width fill, paddingEach { left = 18, right = 0, top = 0, bottom = 0 } ]
        [ el [ height fill ] none
        , column [ width fill ]
            [ row [ width fill, spacing 8 ]
                [ itemSymbol
                , row [ width fill ] (List.map (renderBlock generation settings) blocks)
                ]
            ]
        ]


itemSymbol =
    el [ Font.bold, alignTop, moveUp 1, Font.size 18 ] (text "•")


codeColor =
    -- E.rgb 0.2 0.5 1.0
    rgb 0.4 0 0.8


notImplemented str =
    el [ Font.color (rgb255 40 40 255) ] (text <| "not implemented: " ++ str)
