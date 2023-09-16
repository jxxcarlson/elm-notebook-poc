port module Main exposing (main)

{- This is a starter app which presents a text label, text field, and a button.
   What you enter in the text field is echoed in the label.  When you press the
   button, the text in the label is reverse.
   This version uses `mdgriffith/elm-ui` for the view functions.
-}

import Browser
import Codec
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import ErrorReporter
import Eval
import File.Download as Download
import Html exposing (Html)
import Json.Decode exposing (Value)
import Keyboard
import Types exposing (Msg(..), ReplData)


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { input : String
    , output : List ErrorReporter.MessageItem
    , replData : Maybe ReplData
    , evalState : Eval.EvalState
    , pressedKeys : List Keyboard.Key
    }


port sendData : String -> Cmd msg


port receiveData : (Value -> msg) -> Sub msg


type alias Flags =
    {}


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { input = ""
      , output = []
      , replData = Nothing
      , evalState = Eval.initEvalState
      , pressedKeys = []
      }
    , Cmd.none
    )


subscriptions _ =
    Sub.batch
        [ receiveData ReceivedDataFromJS
        , Sub.map KeyboardMsg Keyboard.subscriptions
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        InputText str ->
            ( { model | input = str }, Cmd.none )

        RequestEval ->
            ( { model | replData = Nothing }, Eval.submitExpression model.evalState model.input )

        GotReply result ->
            case result of
                Ok str ->
                    if String.left 24 str == "{\"type\":\"compile-errors\"" then
                        let
                            report =
                                case ErrorReporter.decodeErrorReporter str of
                                    Ok report_ ->
                                        report_
                                            |> .errors
                                            |> List.concatMap .problems
                                            |> List.concatMap .message

                                    --|> List.map ErrorReporter.renderMessageItem
                                    --|> String.join "\n\n"
                                    --|> (\items -> column [Font.size 12, width (px 600)] (List.map text items))
                                    Err _ ->
                                        [ ErrorReporter.Plain "Oops, something wen't wrong." ]
                        in
                        ( { model | output = report }, Cmd.none )

                    else
                        ( { model | output = [ ErrorReporter.stringToMessageItem "Ok" ] }, sendData str )

                Err _ ->
                    ( { model | output = [ ErrorReporter.stringToMessageItem "Error" ] }, Cmd.none )

        ReceivedDataFromJS value ->
            case Codec.decodeValue Eval.replDataCodec value of
                Ok data ->
                    ( { model | replData = Just data }, Cmd.none )

                Err _ ->
                    ( { model | replData = Nothing }, Cmd.none )

        KeyboardMsg keyMsg ->
            let
                pressedKeys =
                    Keyboard.update keyMsg model.pressedKeys

                ( newModel, cmd ) =
                    -- TODO: cmd?
                    if List.member Keyboard.Shift pressedKeys && List.member Keyboard.Enter pressedKeys then
                        ( { model | replData = Nothing }, Eval.submitExpression model.evalState model.input )

                    else
                        ( { model | pressedKeys = pressedKeys }, Cmd.none )
            in
            ( newModel, cmd )


download : String -> Cmd msg
download jsContent =
    Download.string "reply.js" "text/javascriptl" jsContent



--
-- VIEW
--


view : Model -> Html Msg
view model =
    Element.layout [ Background.color (rgb 0 0 0) ] (mainColumn model)


mainColumn : Model -> Element Msg
mainColumn model =
    column mainColumnStyle
        [ column [ centerX, spacing 20, width (px 600), height (px 400) ]
            [ title "Elm Notebook POC"
            , column [ spacing 4, width (px 600) ]
                [ inputText model
                , el [ Font.size 12, Font.italic ] (text "Shift + Enter to evaluate cell")
                ]
            , display model

            --, appButton
            , if List.isEmpty model.output then
                Element.none

              else
                paragraph [ paddingXY 8 8, Font.color (rgb 0.9 0.9 0.9), Font.size 14, width (px 600), Background.color (rgb 0 0 0) ] (List.map ErrorReporter.renderMessageItem model.output)
            , displayReturnValue model
            ]
        ]


title : String -> Element msg
title str =
    row [ centerX, Font.bold ] [ text str ]


display : Model -> Element msg
display model =
    let
        value =
            case model.replData of
                Nothing ->
                    ""

                Just replData ->
                    replData.value
    in
    row [ Font.size 16 ]
        [ text value ]


displayReturnValue : Model -> Element msg
displayReturnValue model =
    case model.replData of
        Nothing ->
            row [ Font.size 12, alignBottom ]
                [ text "" ]

        Just replData ->
            column [ Font.size 12, spacing 12, alignBottom ]
                [ text <| "name = " ++ (replData.name |> Maybe.withDefault "none")
                , text <| "value = " ++ replData.value
                , text <| "type = " ++ replData.tipe
                ]


inputText : Model -> Element Msg
inputText model =
    Input.text []
        { onChange = InputText
        , text = model.input
        , placeholder = Just <| Input.placeholder [] (text "Enter text here")
        , label = Input.labelHidden "Input text"
        }


appButton : Element Msg
appButton =
    row []
        [ Input.button buttonStyle
            { onPress = Just RequestEval
            , label = el [ centerY ] (text "Eval")
            }
        ]



--
-- STYLE
--


mainColumnStyle =
    [ centerX
    , centerY
    , Background.color (rgb255 240 240 240)
    , paddingXY 20 20
    ]


buttonStyle =
    [ Background.color (rgb255 40 40 40)
    , Font.color (rgb255 255 255 255)
    , paddingXY 15 8
    , Font.size 12
    ]
