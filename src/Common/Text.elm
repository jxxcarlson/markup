module Common.Text exposing (render)

import Common.Syntax as Syntax exposing (Block(..), Text(..), TextBlock(..))
import Dict exposing (Dict)
import Element exposing (Element)
import Element.Background as Background
import Element.Font as Font


type alias Settings =
    { width : Int }


render : Int -> Settings -> Text -> Element msg
render generation settings text =
    case text of
        Text string meta ->
            Element.el [] (Element.text string)

        Marked name textList meta ->
            Element.el [] (renderMarked name generation settings textList)

        Verbatim name textList meta ->
            Element.paragraph [] (List.map (render generation settings) textList)

        Arg _ _ ->
            Element.none

        TError error_ ->
            error error_


error str =
    Element.paragraph [ Background.color (Element.rgb255 250 217 215) ] [ Element.text str ]


notImplemented str =
    Element.el [ Font.color (Element.rgb255 40 40 255) ] (Element.text <| "not implemented: " ++ str)


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
        , ( "#", \g s textList -> heading1 g s textList )
        , ( "##", \g s textList -> heading2 g s textList )
        , ( "###", \g s textList -> heading3 g s textList )
        , ( "####", \g s textList -> heading4 g s textList )
        , ( "*", \g s textList -> strong g s textList )
        , ( "_", \g s textList -> italic g s textList )
        ]


simpleElement formatList g s textList =
    Element.paragraph formatList (List.map (render g s) textList)


heading1 g s textList =
    -- simpleElement [ Font.size 32, Font.color (Element.rgb255 200 0 0) ] g s textList
    Element.paragraph [ Font.size 32 ] (List.map (render g s) textList)


heading2 g s textList =
    simpleElement [ Font.size 22 ] g s textList


heading3 g s textList =
    simpleElement [ Font.size 16 ] g s textList


heading4 g s textList =
    simpleElement [ Font.size 14 ] g s textList


strong : Int -> Settings -> List Text -> Element msg
strong g s textList =
    Element.paragraph [ Font.bold ] (List.map (render g s) textList)


italic : Int -> Settings -> List Text -> Element msg
italic g s textList =
    Element.paragraph [ Font.italic, Element.paddingEach { left = 0, right = 2, top = 0, bottom = 0 } ] (List.map (render g s) textList)


red : Int -> Settings -> List Text -> Element msg
red g s textList =
    Element.paragraph [ Font.color (Element.rgb255 200 0 0) ] (List.map (render g s) textList)
