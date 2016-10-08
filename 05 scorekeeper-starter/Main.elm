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



-- alternatively, "editing" could be put as a list in the top level of the Model
-- (passing it a Maybe playerId and True/False (actually, the Maybe playerId should be enough) )


type alias Player =
    { id : Int
    , name : String
    , points : Int
    , editing : Bool
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
            let
                newPlayers =
                    List.map
                        (\player ->
                            if player.editing == True then
                                { player | editing = False }
                            else
                                player
                        )
                        model.players
            in
                { model | players = newPlayers, playerName = "", playerId = Nothing }

        Save ->
            -- duplication already
            let
                newPlayers =
                    List.map
                        (\player ->
                            if player.editing == True then
                                { player | editing = False }
                            else
                                player
                        )
                        model.players
            in
                if (String.isEmpty model.playerName) then
                    { model | players = newPlayers }
                else
                    save { model | players = newPlayers }

        Edit player ->
            let
                newPlayers =
                    List.map
                        (\p ->
                            if p.id == player.id then
                                { p | editing = True }
                            else
                                p
                        )
                        model.players
            in
                { model | players = newPlayers, playerName = player.name, playerId = Just player.id }

        Score player points ->
            score model player points

        DeletePlay play ->
            deletePlay model play


deletePlay : Model -> Play -> Model
deletePlay model playToDelete =
    let
        newPlays =
            List.filter (\play -> play.id /= playToDelete.id) model.plays

        newPlayers =
            List.map
                (\player ->
                    if player.id == playToDelete.playerId then
                        { player | points = player.points - playToDelete.points }
                    else
                        player
                )
                model.players
    in
        { model
            | plays = newPlays
            , players = newPlayers
        }


score : Model -> Player -> Int -> Model
score model scorer points =
    let
        newPlayers =
            List.map
                (\player ->
                    if player.id == scorer.id then
                        { player | points = player.points + points }
                    else
                        player
                )
                model.players

        newPlay =
            Play (List.length model.plays + 1) scorer.id scorer.name points

        newPlays =
            newPlay :: model.plays
    in
        { model
            | players = newPlayers
            , plays = newPlays
        }


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
            Player playerId model.playerName 0 False

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
        , playerSection model
        , playerForm model
        , playSection model
        , text (toString model)
        ]


playSection : Model -> Html Msg
playSection model =
    div []
        [ playListHeader
        , playList model
        ]


playListHeader : Html Msg
playListHeader =
    header []
        [ div [] [ text "Plays" ]
        , div [] [ text "Points" ]
        ]


playList : Model -> Html Msg
playList model =
    model.plays
        |> List.take 5
        |> List.map play
        |> ul []


play : Play -> Html Msg
play play =
    li []
        [ i
            [ class "remove"
            , onClick (DeletePlay play)
            ]
            []
        , div []
            [ text play.playerName ]
        , div []
            [ text (toString play.points) ]
        ]


playerSection : Model -> Html Msg
playerSection model =
    div []
        [ playerListHeader
        , playerList model
        , pointTotal model
        ]


playerListHeader : Html Msg
playerListHeader =
    header []
        [ div [] [ text "Name" ]
        , div [] [ text "Points" ]
        ]


playerList : Model -> Html Msg
playerList model =
    -- ul []
    --     (List.map player model.players)
    model.players
        |> List.sortBy .name
        |> List.map player
        |> ul []


player : Player -> Html Msg
player player =
    let
        playerClass =
            if player.editing == True then
                "edit"
            else
                ""
    in
        li []
            [ i
                [ class "edit"
                , onClick (Edit player)
                ]
                []
            , div [ class playerClass ]
                [ text player.name ]
            , button
                [ type' "button"
                , onClick (Score player 2)
                ]
                [ text "2pt" ]
            , button
                [ type' "button"
                , onClick (Score player 3)
                ]
                [ text "3pt" ]
            , div []
                [ text (toString player.points) ]
            ]


pointTotal : Model -> Html Msg
pointTotal model =
    let
        total =
            List.map .points model.players
                |> List.sum
    in
        footer []
            [ div [] [ text "Total:" ]
            , div [] [ text (toString total) ]
            ]


playerForm : Model -> Html Msg
playerForm model =
    let
        editingList =
            List.map .editing model.players
                |> List.filter (\val -> val == True)

        isEditingClass =
            if editingList == [] then
                ""
            else
                "edit"
    in
        Html.form [ onSubmit Save ]
            [ input
                [ type' "text"
                , placeholder "Add/Edit player.."
                , onInput Input
                , value model.playerName
                , class isEditingClass
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
