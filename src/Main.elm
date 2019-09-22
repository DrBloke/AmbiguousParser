module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import ChemParser exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)


main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model =
    { textInput : String }


init : Model
init =
    { textInput = "" }


type Msg
    = ChangeInput String
    | NoOp


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeInput inputText ->
            let
                formula =
                    if inputText == "" then
                        ""

                    else
                        case runParseFormula inputText of
                            Err error ->
                                "Invalid formula " ++ Debug.toString error

                            Ok value ->
                                Debug.toString value
            in
            { model | textInput = formula }

        NoOp ->
            model


view : Model -> Html Msg
view model =
    div []
        [ input [ type_ "text", placeholder "Enter formula", onInput ChangeInput ] []
        , br [] []
        , text model.textInput
        ]
