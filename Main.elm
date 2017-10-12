module Main exposing (..)

import Html exposing (Html, div, textarea, text)
import Html.Events exposing (onInput)
import Html.Attributes exposing (style)
import Interpreter


main : Program Never Model Msg
main =
    Html.beginnerProgram { model = model, view = view, update = update }


type alias Model =
    { code : String
    }


model : Model
model =
    { code = "" }


interpreterOutput : Result String Int -> String
interpreterOutput output =
    case output of
        Ok val ->
            toString val

        Err msg ->
            "Error: " ++ msg


view : Model -> Html Msg
view model =
    div []
        [ textarea
            [ onInput SetCode
            , style
                [ ( "width", "40em" )
                , ( "height", "30em" )
                ]
            ]
            []
        , div [] [ text (model.code |> Interpreter.interpret |> interpreterOutput) ]
        ]


type Msg
    = SetCode String


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetCode code ->
            { model | code = code }
