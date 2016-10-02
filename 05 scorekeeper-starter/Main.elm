module Main exposing (..)

import String
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Html.App as App


-- model


type alias Model =
    { players : List Player
    , playerName : String
    , playerId : Maybe Int
    , plays : List Play
    }


type alias Player =
    { id : Int
    , name : String
    , points : Int
    }


type alias Play =
    { id : Int
    , playerId : Int
    , playerName : String
    , points : Int
    }


initModel : Model
initModel =
    { players = []
    , playerName = ""
    , playerId = Nothing
    , plays = []
    }



-- update


type Msg
    = Edit Player
    | Score Player Int
    | Input String
    | Save
    | Cancel
    | DeletePlay Play


update : Msg -> Model -> Model
update msg model =
    case msg of
        Input val ->
            { model | playerName = val }

        Cancel ->
            { model | playerName = "", playerId = Nothing }

        Save ->
            if (String.isEmpty model.playerName) then
                model
            else
                save model

        _ ->
            model


save : Model -> Model
save model =
    case model.playerId of
        Just id ->
            edit model id

        -- if model.playerId == Nothing then
        Nothing ->
            add model


edit : Model -> Int -> Model
edit model id =
    let
        newPlayers =
            List.map
                (\player ->
                    if player.id == id then
                        { player | name = model.playerName }
                    else
                        player
                )
                model.players

        newPlays =
            List.map
                (\play ->
                    if play.playerId == id then
                        { play | playerName = model.playerName }
                    else
                        play
                )
                model.plays
    in
        { model
            | players = newPlayers
            , plays = newPlays
            , playerName = ""
            , playerId = Nothing
        }


add : Model -> Model
add model =
    let
        playerId =
            (List.length model.players) + 1

        newPlayer =
            -- { id = playerId, name = model.playerName, points = 0 }
            Player playerId model.playerName 0

        newPlayers =
            -- ++ operator adds to the end of the List, but is also more expensive
            -- model.players ++ [ newPlayer ]
            -- :: (con operator) adds to the beginning of the List
            newPlayer :: model.players
    in
        { model
            | players = newPlayers
            , playerName = ""
        }



-- view


view : Model -> Html Msg
view model =
    div [ class "scoreboard" ]
        [ h1 [] [ text "Score Keeper" ]
        , playerForm model
        , p [] [ text (toString model) ]
        ]


playerForm : Model -> Html Msg
playerForm model =
    Html.form [ onSubmit Save ]
        [ input
            [ type' "text"
            , placeholder "Add/Edit player.."
            , onInput Input
            , value model.playerName
            ]
            []
        , button
            [ type' "submit" ]
            [ text "Save" ]
        , button
            [ type' "button"
            , onClick Cancel
            ]
            [ text "Cancel" ]
        ]


main : Program Never
main =
    App.beginnerProgram
        { model = initModel
        , update = update
        , view = view
        }
