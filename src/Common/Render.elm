module Common.Render exposing (Settings, render)

import Common.Syntax as Syntax exposing (Block(..))
import Dict exposing (Dict)
import Element exposing (Element)
import Element.Font as Font


type alias Settings =
    { width : Int }


render : Int -> Settings -> List Block -> List (Element msg)
render generation settings blocks =
    List.map (renderBlock generation settings) blocks


renderBlock : Int -> Settings -> Block -> Element msg
renderBlock generation settings block =
    case block of
        Paragraph strings _ ->
            Element.paragraph [] (List.map Element.text strings)

        VerbatimBlock name lines _ ->
            case Dict.get name verbatimBlockDict of
                Nothing ->
                    Element.el [] (Element.text ("Unimplemented verbatim block: " ++ name))

                Just f ->
                    f generation settings lines

        Block name blocks _ ->
            case Dict.get name blockDict of
                Nothing ->
                    Element.el [] (Element.text ("Unimplemented block: " ++ name))

                Just f ->
                    f generation settings blocks


verbatimBlockDict : Dict String (Int -> Settings -> List String -> Element msg)
verbatimBlockDict =
    Dict.fromList
        [ ( "code", \g s lines -> codeBlock g s lines )
        ]


blockDict : Dict String (Int -> Settings -> List Block -> Element msg)
blockDict =
    Dict.fromList
        [ ( "quotation", \g s blocks -> quotationBlock g s blocks )
        ]


codeBlock : Int -> Settings -> List String -> Element msg
codeBlock generation settings lines =
    Element.column
        [ Font.family
            [ Font.typeface "Inconsolata"
            , Font.monospace
            ]
        , Font.color codeColor
        , Element.paddingEach { left = 18, right = 0, top = 0, bottom = 0 }
        ]
        (List.map Element.text lines)


quotationBlock : Int -> Settings -> List Block -> Element msg
quotationBlock generation settings blocks =
    Element.column
        [ Element.paddingEach { left = 18, right = 0, top = 0, bottom = 0 }
        ]
        (List.map (renderBlock generation settings) blocks)


codeColor =
    -- E.rgb 0.2 0.5 1.0
    Element.rgb 0.4 0 0.8


notImplemented str =
    Element.el [ Font.color (Element.rgb255 40 40 255) ] (Element.text <| "not implemented: " ++ str)
