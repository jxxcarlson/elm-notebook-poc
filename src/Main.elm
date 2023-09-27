port module Main exposing (main)

{- This is a starter app which presents a text label, text field, and a button.
   What you enter in the text field is echoed in the label.  When you press the
   button, the text in the label is reverse.
   This version uses `mdgriffith/elm-ui` for the view functions.
-}

import Browser
import Codec
import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import ErrorReporter exposing (MessageItem(..))
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
    { expressionText : String
    , report : List ErrorReporter.MessageItem
    , replData : Maybe ReplData
    , evalState : Eval.EvalState
    , pressedKeys : List Keyboard.Key
    }


port sendDataToJS : String -> Cmd msg


port receiveFromJS : (Value -> msg) -> Sub msg


type alias Flags =
    {}


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { expressionText = ""
      , report = []
      , replData = Nothing
      , evalState = Eval.initEmptyEvalState
      , pressedKeys = []
      }
    , Cmd.none
    )


subscriptions _ =
    Sub.batch
        [ receiveFromJS ReceivedFromJS
        , Sub.map KeyboardMsg Keyboard.subscriptions
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        InputText str ->
            ( { model | expressionText = str }, Cmd.none )

        RequestEval ->
            ( { model | replData = Nothing }, Eval.requestEvaluation model.evalState model.expressionText )

        GotReply result ->
            case result of
                Ok str ->
                    if Eval.hasReplError str then
                        ( { model | report = Eval.reportError str }, Cmd.none )

                    else
                        ( { model | report = [ ErrorReporter.stringToMessageItem "Ok" ] }, sendDataToJS str )

                Err _ ->
                    ( { model | report = [ ErrorReporter.stringToMessageItem "Error" ] }, Cmd.none )

        ReceivedFromJS value ->
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
                    if List.member Keyboard.Shift pressedKeys && List.member Keyboard.Enter pressedKeys then
                        case String.split "=" model.expressionText of
                            [] ->
                                ( model, Cmd.none )

                            expr :: [] ->
                                processExpr model expr

                            name :: expr :: [] ->
                                let
                                    newEvalState =
                                        Eval.insertDeclaration name (name ++ " = " ++ expr ++ "\n") model.evalState

                                    replData =
                                        Just { name = Nothing, value = "Ok", tipe = "" }
                                in
                                ( { model | replData = replData, evalState = newEvalState }, Cmd.none )

                            _ ->
                                ( { model | pressedKeys = pressedKeys }, Cmd.none )

                    else
                        ( { model | pressedKeys = pressedKeys }, Cmd.none )
            in
            ( newModel, cmd )


processExpr model expr =
    if String.left 7 expr == ":remove" then
        let
            key =
                String.dropLeft 8 expr |> String.trim
        in
        case Dict.get key model.evalState.decls of
            Just _ ->
                ( { model
                    | replData = Just { name = Nothing, value = key ++ ": removed", tipe = "" }
                    , evalState = Eval.removeDeclaration key model.evalState |> Debug.log "DICT"
                  }
                , Cmd.none
                )

            Nothing ->
                ( { model | replData = Just { name = Nothing, value = key ++ ": not found", tipe = "" } }, Cmd.none )

    else
        ( { model | replData = Nothing }, Eval.requestEvaluation model.evalState expr )


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
        [ column
            [ centerX
            , spacing 20
            , width (px 600)
            , height (px 600)
            ]
            [ title "Elm Notebook POC"
            , column [ spacing 4, width (px 600) ]
                [ inputText model
                , el [ Font.size 12, Font.italic ] (text "Shift + Enter to evaluate cell; foo = 1 to add foo to dictionary; : remove foo to remove foo from dictionary")
                ]
            , display model
            , Eval.displayDictionary model.evalState.decls

            --, appButton
            , ErrorReporter.render model.report
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
                    case replData.name of
                        Nothing ->
                            replData.value

                        Just name ->
                            name ++ " = " ++ replData.value
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
        , text = model.expressionText
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
