module Main exposing (main)

import Browser
import Common.API as API
import Data.Article
import Data.Article2
import Data.Example
import Data.MarkdownTest
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import File.Download as Download
import Html exposing (Html)
import Process
import Task


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { sourceText : String
    , docIdentifier : String
    , count : Int
    , windowHeight : Int
    , windowWidth : Int
    }


type Msg
    = NoOp
    | InputText String
    | ClearText
    | LoadDocumentText String String
    | IncrementCounter


type alias Flags =
    { width : Int, height : Int }


initialText =
    Data.MarkdownTest.text


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { sourceText = initialText
      , docIdentifier = "markdown"
      , count = 0
      , windowHeight = flags.height
      , windowWidth = flags.width
      }
    , Process.sleep 100 |> Task.perform (always IncrementCounter)
    )


renderArgs =
    { width = 450
    , selectedId = "foobar"
    , generation = 0
    }


subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        InputText str ->
            ( { model
                | sourceText = str -- String.trim str
                , count = model.count + 1
              }
            , Cmd.none
            )

        ClearText ->
            ( { model
                | sourceText = ""
                , count = model.count + 1
              }
            , Cmd.none
            )

        LoadDocumentText docIdentifier text ->
            ( { model | sourceText = text, docIdentifier = docIdentifier, count = model.count + 1 }, Cmd.none )

        IncrementCounter ->
            ( model, Cmd.none )


download : String -> String -> String -> Cmd msg
download filename mimetype content =
    Download.string filename mimetype content



--
-- VIEW
--


view : Model -> Html Msg
view model =
    Element.layoutWith { options = [ focusStyle noFocus ] } [ bgGray 0.2, clipX, clipY ] (mainColumn model)


noFocus : Element.FocusStyle
noFocus =
    { borderColor = Nothing
    , backgroundColor = Nothing
    , shadow = Nothing
    }


mainColumn : Model -> Element Msg
mainColumn model =
    column (mainColumnStyle model)
        [ column [ spacing 48, width (px appWidth_), height (px (appHeight_ model)) ]
            [ title "L1 Demo App"
            , column [ spacing 12 ]
                [ row [ spacing 12 ] [ editor model, rhs model ]
                ]
            , row [ Font.size 14, Font.color whiteColor ] []
            ]
        ]


editor model =
    column [ spacing 8, moveUp 9 ]
        [ row [ spacing 12 ]
            [ exampleDocButton model.docIdentifier
            , articleButton model.docIdentifier
            ]
        , inputText model
        ]


keyIt : Int -> List b -> List ( String, b )
keyIt k list =
    List.indexedMap (\i e -> ( String.fromInt (i + k), e )) list


title : String -> Element msg
title str =
    row [ centerX, fontGray 0.9 ] [ text str ]


rhs : Model -> Element Msg
rhs model =
    column [ spacing 8 ]
        [ row
            [ fontGray 0.9
            , spacing 12
            , moveUp 9
            , Font.size 14
            ]
            [ dummyButton, text ("generation: " ++ String.fromInt model.count), wordCountElement model.sourceText ]
        , renderedText model
        ]


wordCount : String -> Int
wordCount str =
    str |> String.words |> List.length


wordCountElement : String -> Element Msg
wordCountElement str =
    row [ spacing 8 ] [ el [] (text <| "words:"), el [] (text <| String.fromInt <| wordCount <| str) ]


renderedText : Model -> Element Msg
renderedText model =
    column
        [ spacing 18
        , paddingXY 24 36
        , width (px panelWidth_)
        , height (px (panelHeight_ model))
        , scrollbarY
        , moveUp 9
        , Font.size 12
        , alignTop
        , Background.color (Element.rgb255 255 240 240)
        ]
        (API.renderMarkdown 0 { width = 500 } (String.lines model.sourceText))



-- INPUT


inputText : Model -> Element Msg
inputText model =
    Input.multiline [ height (px (panelHeight_ model)), width (px panelWidth_), Font.size 14 ]
        { onChange = InputText
        , text = model.sourceText
        , placeholder = Nothing
        , label = Input.labelHidden "Enter source text here"
        , spellcheck = False
        }



-- BUTTONS


defaultButtonColor =
    Element.rgb255 60 60 60


buttonColor buttonMode currentMode =
    if buttonMode == currentMode then
        Element.rgb255 130 12 9

    else
        Element.rgb255 60 60 60


clearTextButton : Element Msg
clearTextButton =
    Input.button buttonStyle2
        { onPress = Just ClearText
        , label = el [ centerX, centerY, Font.size 14 ] (text "Clear")
        }


exampleDocButton : String -> Element Msg
exampleDocButton docIdentifier =
    Input.button (activeButtonStyle (docIdentifier == "examples"))
        { onPress = Just (LoadDocumentText "examples" Data.Example.text)
        , label = el [ centerX, centerY, Font.size 14 ] (text "Examples")
        }


articleButton : String -> Element Msg
articleButton docIdentifier =
    Input.button (activeButtonStyle (docIdentifier == "article"))
        { onPress = Just (LoadDocumentText "article" Data.Article2.text)
        , label = el [ centerX, centerY, Font.size 14 ] (text "Article")
        }


dummyButton : Element Msg
dummyButton =
    row [ Background.color defaultButtonColor ]
        [ Input.button buttonStyle
            { onPress = Nothing
            , label = el [ centerX, centerY, Font.size 14 ] (text "Rendered text")
            }
        ]



-- PARAMETERS


widePanelWidth_ =
    2 * panelWidth_


panelWidth_ =
    520


appHeight_ model =
    model.windowHeight - 300


panelHeight_ model =
    appHeight_ model - parserDisplayPanelHeight_ - 100


parserDisplayPanelHeight_ =
    0


appWidth_ =
    2 * panelWidth_ + 15



--
-- STYLE
--


mainColumnStyle model =
    [ centerX
    , centerY
    , bgGray 0.5
    , paddingXY 20 20
    , width (px (appWidth_ + 40))
    , height (px (appHeight_ model + 40))
    ]


buttonStyle =
    [ Font.color (rgb255 255 255 255)
    , paddingXY 15 8
    ]


buttonStyle2 =
    [ Font.color (rgb255 255 255 255)
    , Background.color (rgb255 0 0 160)
    , paddingXY 15 8
    , mouseDown [ Background.color (rgb255 180 180 255) ]
    ]


activeButtonStyle isSelected =
    if isSelected then
        [ Font.color (rgb255 255 255 255)
        , Background.color (rgb255 140 0 0)
        , paddingXY 15 8
        , mouseDown [ Background.color (rgb255 255 180 180) ]
        ]

    else
        [ Font.color (rgb255 255 255 255)
        , Background.color (rgb255 0 0 160)
        , paddingXY 15 8
        , mouseDown [ Background.color (rgb255 180 180 255) ]
        ]


grayColor g =
    Element.rgb g g g


whiteColor =
    grayColor 1


fontGray g =
    Font.color (Element.rgb g g g)


bgGray g =
    Background.color (Element.rgb g g g)
