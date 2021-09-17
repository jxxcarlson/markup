module Common.RenderText exposing (..)

import Common.Syntax as Syntax exposing (Text(..))
import Element exposing (Element)
import Element.Background as Background


render : Text -> Element msg
render text =
    case text of
        Text strings meta ->
            Element.column [ Element.spacing 24 ] (List.map (\p -> Element.paragraph [ Element.spacing 6 ] [ Element.text p ]) (prepare strings))

        Marked name textList meta ->
            Element.paragraph [] (List.map render textList)

        Verbatim name textList meta ->
            Element.paragraph [] (List.map render textList)

        TError error_ ->
            error error_


error str =
    Element.paragraph [ Background.color (Element.rgb255 250 217 215) ] [ Element.text str ]


prepare : List String -> List String
prepare strings =
    strings |> List.map reflate |> String.join " " |> String.trim |> String.split "\n"


reflate : String -> String
reflate str =
    if str == "" then
        "\n"

    else
        str
