module Common.Render exposing (Settings, render)

import Common.Syntax as Syntax exposing (Block(..))
import Dict exposing (Dict)
import Element exposing (Element)
import Element.Font as Font
import Html exposing (Html)
import Html.Attributes as HA
import Html.Keyed
import Json.Decode
import Json.Encode


type alias Settings =
    { width : Int }


render : Int -> Settings -> List Block -> List (Element msg)
render generation settings blocks =
    List.map (renderBlock generation settings) blocks


renderBlock : Int -> Settings -> Block -> Element msg
renderBlock generation settings block =
    case block of
        Paragraph strings _ ->
            Element.column [ Element.spacing 24 ] (List.map (\p -> Element.paragraph [ Element.spacing 6 ] [ Element.text p ]) (prepare strings))

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


prepare : List String -> List String
prepare strings =
    strings |> List.map reflate |> String.join " " |> String.split "\n"


reflate : String -> String
reflate str =
    if str == "" then
        "\n"

    else
        str


verbatimBlockDict : Dict String (Int -> Settings -> List String -> Element msg)
verbatimBlockDict =
    Dict.fromList
        [ ( "code", \g s lines -> codeBlock g s lines )
        , ( "math", \g s lines -> mathBlock g s lines )
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


mathBlock : Int -> Settings -> List String -> Element msg
mathBlock generation settings strings =
    mathText generation DisplayMathMode (String.join "\n" strings)


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
