module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App as App
import String


-- model


type alias Model =
    { currentCalories : Int, input : Int, error : Maybe String }


initModel : Model
initModel =
    -- { currentCalories = 0, caloriesToAdd = 0 } - full way of initializing
    Model 0 0 Nothing



-- update


type Msg
    = AddCalories
    | Input String
    | Clear


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddCalories ->
            { model
                | currentCalories = model.currentCalories + model.input
                , input = 0
            }

        Input val ->
            case String.toInt val of
                Ok input ->
                    { model
                        | input = input
                        , error = Nothing
                    }

                Err err ->
                    { model
                        | input = 0
                        , error = Just err
                    }

        Clear ->
            initModel



-- view


view : Model -> Html Msg
view model =
    div []
        [ h3 []
            [ text ("Total Calories: " ++ (toString model.currentCalories)) ]
        , input
            [ type' "number"
            , onInput Input
            , value
                (if model.input == 0 then
                    ""
                 else
                    toString model.input
                )
            ]
            []
        , br [] []
        , button
            [ type' "button"
            , onClick AddCalories
            ]
            [ text "Add" ]
        , button
            [ type' "button"
            , onClick Clear
            ]
            [ text "Clear" ]
        ]


main : Program Never
main =
    App.beginnerProgram
        { model = initModel
        , update = update
        , view = view
        }
