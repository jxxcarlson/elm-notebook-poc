port module Main exposing (main)

{- This is a starter app which presents a text label, text field, and a button.
   What you enter in the text field is echoed in the label.  When you press the
   button, the text in the label is reverse.
   This version uses `mdgriffith/elm-ui` for the view functions.
-}

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Eval
import Html exposing (Html)
import Types exposing (Msg(..), ReplData)
import File.Download as Download
import Json.Decode exposing (Value)
import Codec
import Keyboard

main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { input : String
    , output : String
    , replData : Maybe ReplData
    , pressedKeys : List Keyboard.Key
    }



port sendData : String -> Cmd msg

port receiveData : (Value -> msg) -> Sub msg

type alias Flags =
    {}


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { input = ""
      , output = ""
      , replData = Nothing
      , pressedKeys = []
      }
    , Cmd.none
    )



subscriptions _ =
        Sub.batch [receiveData ReceivedDataFromJS
        , Sub.map KeyboardMsg Keyboard.subscriptions]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        InputText str ->
            ( { model | input = str }, Cmd.none )

        RequestEval ->
            ( model, Eval.submitExpression model.input )


        GotReply result ->

            case result of
                Ok str ->
                    if String.left 24 str == "{\"type\":\"compile-errors\""
                    then
                        let
                            _ = Debug.log "STR" str
                        in
                        ( { model | output = str }, Cmd.none )
                    else
                    ( { model | output =  "Ok" },  sendData str )

                Err _ ->
                    ( { model | output = "Error" }, Cmd.none )

        ReceivedDataFromJS value ->
            case Codec.decodeValue Eval.replDataCodec value of
                Ok data ->
                    ( { model | replData = Just data}, Cmd.none )

                Err _ ->
                      ( { model | replData = Nothing}, Cmd.none )

        KeyboardMsg keyMsg ->
            let
                pressedKeys =
                    Keyboard.update keyMsg model.pressedKeys

                ( newModel, cmd ) =
                    -- TODO: cmd?
                    if List.member Keyboard.Shift pressedKeys && List.member Keyboard.Enter pressedKeys then
                        (model, Eval.submitExpression model.input)

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
    Element.layout [Background.color (rgb 0 0 0)] (mainColumn model)


mainColumn : Model -> Element Msg
mainColumn model =
    column mainColumnStyle
        [ column [ centerX, spacing 20, width (px 600), height (px 400) ]
            [ title "Elm Notebook POC"
            , column [spacing 4, width (px 600)] [inputText model
                , el [Font.size 12, Font.italic] (text "Shift + Enter to evaluate cell")
                ]
            , display model
            --, appButton
            , paragraph [Font.size 12, width (px 600)] [text model.output]

            , displayReturnValue model
            ]
        ]


title : String -> Element msg
title str =
    row [ centerX, Font.bold ] [ text str ]

display : Model -> Element msg
display model =
  let
    value = case model.replData of
         Nothing -> "Error"
         Just replData -> replData.value
   in
        row [Font.size 16  ]
        [ text value ]


displayReturnValue : Model -> Element msg
displayReturnValue model =
    case model.replData of
             Nothing ->
                 row [Font.size 12 , alignBottom ]
                         [ text "Error" ]
             Just replData ->
               column [Font.size 12, spacing 12,  alignBottom ]
                 [ text <| "name = "   ++ (replData.name |> Maybe.withDefault "none")
                 , text <|  "value = " ++ replData.value
                 , text <| "type = "   ++ replData.tipe
                 ]



inputText : Model -> Element Msg
inputText model =
    Input.text []
        { onChange = InputText
        , text = model.input
        , placeholder = Just <| Input.placeholder [ ] (text "Enter text here")
        , label = Input.labelHidden "Input text"
        }


appButton : Element Msg
appButton =
    row [  ]
        [ Input.button buttonStyle
            { onPress = Just RequestEval
            , label = el [  centerY ] (text "Eval")
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
