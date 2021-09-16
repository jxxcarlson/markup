module Common.Render exposing (Settings, render)

import Common.Syntax as Syntax exposing (Block(..), BlockM)
import Element exposing (Element)


type alias Settings =
    { width : Int }


render : Settings -> List BlockM -> List (Element msg)
render settings blocks =
    List.map (renderBlock settings) blocks


renderBlock : Settings -> BlockM -> Element msg
renderBlock settings block =
    case block.content of
        Paragraph strings ->
            Element.wrappedRow [] (List.map Element.text strings)

        _ ->
            Element.el [] (Element.text "Not implmented")
