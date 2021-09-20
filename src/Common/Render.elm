module Common.Render exposing (Settings, render)

import Common.Syntax as Syntax exposing (Block(..), Text(..), TextBlock(..))
import Common.Text
import Dict exposing (Dict)
import Element exposing (Element)
import Element.Background as Background
import Element.Font as Font
import Html exposing (Html)
import Html.Attributes as HA
import Html.Keyed
import Json.Decode
import Json.Encode
import Utility


type alias Settings =
    { width : Int }



-- RENDER


render : Int -> Settings -> List TextBlock -> List (Element msg)
render generation settings blocks =
    List.map (renderBlock generation settings) blocks


renderBlock : Int -> Settings -> TextBlock -> Element msg
renderBlock generation settings block =
    case block of
        TBParagraph textList _ ->
            Element.paragraph
                []
                (List.map (Common.Text.render generation settings) textList)

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
    Element.paragraph [ Background.color (Element.rgb255 250 217 215) ] [ Element.text str ]


verbatimBlockDict : Dict String (Int -> Settings -> List String -> Element msg)
verbatimBlockDict =
    Dict.fromList
        [ ( "code", \g s lines -> codeBlock g s lines )
        , ( "math", \g s lines -> mathBlock g s lines )
        ]


blockDict : Dict String (Int -> Settings -> List TextBlock -> Element msg)
blockDict =
    Dict.fromList
        [ ( "quotation", \g s blocks -> quotationBlock g s blocks )
        ]


codeBlock : Int -> Settings -> List String -> Element msg
codeBlock generation settings textList =
    Element.column
        [ Font.family
            [ Font.typeface "Inconsolata"
            , Font.monospace
            ]
        , Font.color codeColor
        , Element.paddingEach { left = 18, right = 0, top = 0, bottom = 8 }
        ]
        (List.map (\t -> Element.el [] (Element.text t)) textList)


mathBlock : Int -> Settings -> List String -> Element msg
mathBlock generation settings textList =
    mathText generation DisplayMathMode (String.join "\n" textList)


quotationBlock : Int -> Settings -> List Syntax.TextBlock -> Element msg
quotationBlock generation settings blocks =
    Element.column
        [ Element.paddingEach { left = 18, right = 0, top = 0, bottom = 8 }
        ]
        (List.map (renderBlock generation settings) blocks)


codeColor =
    -- E.rgb 0.2 0.5 1.0
    Element.rgb 0.4 0 0.8


notImplemented str =
    Element.el [ Font.color (Element.rgb255 40 40 255) ] (Element.text <| "not implemented: " ++ str)


type DisplayMode
    = InlineMathMode
    | DisplayMathMode


mathText : Int -> DisplayMode -> String -> Element msg
mathText generation displayMode content =
    Html.Keyed.node "span"
        [ HA.style "margin-left" "6px" ]
        [ ( String.fromInt generation, mathText_ displayMode "ID" content )
        ]
        |> Element.html


mathText_ : DisplayMode -> String -> String -> Html msg
mathText_ displayMode selectedId content =
    Html.node "math-text"
        -- active meta selectedId  ++
        [ HA.property "display" (Json.Encode.bool (isDisplayMathMode displayMode))
        , HA.property "content" (Json.Encode.string content)

        -- , clicker meta
        -- , HA.id (makeId meta)
        ]
        []


isDisplayMathMode : DisplayMode -> Bool
isDisplayMathMode displayMode =
    case displayMode of
        InlineMathMode ->
            False

        DisplayMathMode ->
            True
