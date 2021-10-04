module Common.Text.Reduce exposing
    ( argIntoArg
    , argIntoMarked
    , argList
    , contract3Stack
    , contractStackRepeatedly
    , markedIntoArg
    , markedIntoMarked
    , textIntoArg
    , textIntoMarked
    )

import Common.Debug exposing (..)
import Common.Syntax exposing (Expr(..))
import Common.Text
import L1.Transform as Reduce
import List.Extra


argList_ : List Expr -> List Expr
argList_ textList =
    case textList of
        text1 :: text2 :: rest ->
            case ( text1, text2 ) of
                ( Text str1 meta1, Expr name textList2 meta2 ) ->
                    argList <|
                        Expr (name |> debug3 "argList_, name") (Text str1 meta1 :: textList2) { meta2 | end = meta1.end }
                            :: argList (rest |> debug3 "argList_, 1, rest")

                ( Verbatim _ str1 meta1, Expr name textList2 meta2 ) ->
                    argList <|
                        Expr (name |> debug3 "argList_, verbatim name") (Text str1 meta1 :: textList2) { meta2 | end = meta1.end }
                            :: argList (rest |> debug3 "argList_, 2, rest")

                ( Expr name1 textList1 meta1, Expr name2 textList2 meta2 ) ->
                    argList <|
                        Expr name2
                            (Expr (name1 |> debug3 "argList_, 3, name1") textList1 meta1
                                :: textList2
                            )
                            { meta2 | end = meta1.end }
                            :: argList (rest |> debug3 "argList_, 4, rest")

                _ ->
                    textList

        text1 :: [] ->
            textList

        [] ->
            textList


argList : List Expr -> List Expr
argList textList =
    let
        _ =
            debug2 "textList in argList" textList
    in
    case List.Extra.uncons (List.reverse textList) of
        Nothing ->
            []

        Just ( last, rest ) ->
            let
                _ =
                    debug1 "last" last

                _ =
                    debug2 "rest" rest
            in
            case last of
                Expr name textList1 meta ->
                    if Common.Syntax.listSatisfies Common.Syntax.isPureTextOrVerbatim rest then
                        [ Expr (name |> debug3 "argList (X), name") (rest ++ textList1) meta ]

                    else
                        argList_ textList

                _ ->
                    argList_ textList


contract3Stack : List Expr -> List Expr
contract3Stack stack =
    case stack of
        text1 :: text2 :: text3 :: rest ->
            case contract3 text1 text2 text3 of
                Nothing ->
                    stack

                Just text_ ->
                    text_ :: rest

        _ ->
            stack


{-|

    Used in two places:

    - nextCursor, when there is no text left to process. This is natural
    - nextCursor_, in case Commit (this is also natural)

-}
contractStackRepeatedly : List Expr -> List Expr
contractStackRepeatedly stack =
    (case stack of
        text1 :: text2 :: text3 :: rest ->
            case contract3 text1 text2 text3 of
                Nothing ->
                    stack

                Just text_ ->
                    text_ :: rest

        text1 :: text2 :: rest ->
            case contract2 text1 text2 of
                Nothing ->
                    stack

                Just text_ ->
                    contractStackRepeatedly (text_ :: rest)

        _ ->
            stack
    )
        |> List.reverse


argIntoArg : List Expr -> List Expr
argIntoArg stack =
    case stack of
        (Arg textList1 meta1) :: (Arg textList2 meta2) :: rest ->
            Arg (textList1 ++ textList2) { start = meta2.start, end = meta1.end, indent = 0, id = meta2.id } :: rest

        _ ->
            stack


argIntoMarked : List Expr -> List Expr
argIntoMarked stack =
    case stack of
        (Arg textList1 meta1) :: (Expr name textList2 meta2) :: rest ->
            Expr name (textList1 ++ textList2) { start = meta2.start, end = meta1.end, indent = 0, id = meta2.id } :: rest

        _ ->
            stack


markedIntoArg : List Expr -> List Expr
markedIntoArg stack =
    case stack of
        (Expr name textList1 meta1) :: (Arg textList2 meta2) :: rest ->
            Arg [ Expr name textList1 meta1 ] { start = meta2.start, end = meta1.end, indent = 0, id = meta2.id } :: rest

        _ ->
            stack


markedIntoMarked : List Expr -> List Expr
markedIntoMarked stack =
    case stack of
        (Expr name textList1 meta1) :: (Expr name2 textList2 meta2) :: rest ->
            Expr name2 (Expr name textList1 meta1 :: textList2) { start = meta2.start, end = meta1.end, indent = 0, id = meta2.id } :: rest

        _ ->
            stack


textIntoArg : List Expr -> List Expr
textIntoArg stack =
    case stack of
        (Text str meta1) :: (Arg textList2 meta2) :: rest ->
            Arg (Text str meta1 :: textList2) { start = meta2.start, end = meta1.end, indent = 0, id = meta2.id } :: rest

        _ ->
            stack


textIntoMarked : List Expr -> List Expr
textIntoMarked stack =
    case stack of
        (Text str meta1) :: (Expr name textList2 meta2) :: rest ->
            Expr name (Text str meta1 :: textList2) { start = meta2.start, end = meta1.end, indent = 0, id = meta2.id } :: rest

        _ ->
            stack


contract2 : Expr -> Expr -> Maybe Expr
contract2 text1 text2 =
    case ( text1, text2 ) of
        ( Arg textList1 meta1, Arg textList2 meta2 ) ->
            Just <| Arg (textList1 ++ textList2) { start = meta2.start, end = meta1.end, indent = 0, id = meta2.id }

        ( Arg textList1 meta1, Expr name textList2 meta2 ) ->
            Just <| Expr name (textList1 ++ textList2) { start = meta2.start, end = meta1.end, indent = 0, id = meta2.id }

        ( Text str meta1, Arg textList2 meta2 ) ->
            Just <| Arg (Text str meta1 :: textList2) { start = meta2.start, end = meta1.end, indent = 0, id = meta2.id }

        ( Expr name textList1 meta1, Arg textList2 meta2 ) ->
            Just <| Arg (Expr name textList1 meta1 :: textList2) { start = meta2.start, end = meta1.end, indent = 0, id = meta2.id }

        ( Text str meta1, Expr name textList2 meta2 ) ->
            Just <| Expr name (Text str meta1 :: textList2) { start = meta2.start, end = meta1.end, indent = 0, id = meta2.id }

        ( _, _ ) ->
            Nothing


contract3 : Expr -> Expr -> Expr -> Maybe Expr
contract3 text1 text2 text3 =
    case ( text1, text2, text3 ) of
        ( Expr a [] meta1, _, Expr b [] meta3 ) ->
            if a == b then
                Just <| Expr a [ text2 ] { start = meta1.start, end = meta3.end, indent = 0, id = meta3.id }

            else
                Nothing

        ( Verbatim a "" meta1, Text x meta, Verbatim b "" meta3 ) ->
            if a == b then
                Just <| Verbatim a x { start = meta1.start, end = meta3.end, indent = 0, id = meta3.id }

            else
                Nothing

        _ ->
            Nothing
