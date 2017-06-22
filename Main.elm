module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String


-- model


type alias Model =
    { players : List Player
    , name : String
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
    , name : String
    , points : Int
    }


initModel : Model
initModel =
    { players = []
    , name = ""
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
        Input name ->
            Debug.log "Input updated the model "
                { model | name = name }

        Save ->
            if (String.isEmpty model.name) then
                model
            else
                save model

        Cancel ->
            { model
                | name = ""
                , playerId = Nothing
            }

        Score player points ->
            score model player points

        Edit player ->
            { model
                | name = player.name
                , playerId = Just player.id
            }

        DeletePlay play ->
            deletePlay model play


deletePlay : Model -> Play -> Model
deletePlay model deletedPlay =
    let
        newPlays =
            List.filter (\play -> play.id /= deletedPlay.id) model.plays

        newPlayers =
            List.map
                (\player ->
                    if player.id == deletedPlay.playerId then
                        { player | points = player.points - deletedPlay.points }
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
score model scoringPlayer score =
    let
        newPlayers =
            List.map
                (\player ->
                    if player.id == scoringPlayer.id then
                        { player | points = player.points + score }
                    else
                        player
                )
                model.players

        play =
            Play (List.length model.plays) scoringPlayer.id scoringPlayer.name score
    in
        { model
            | players = newPlayers
            , plays = play :: model.plays
        }


save : Model -> Model
save model =
    case model.playerId of
        Just id ->
            editPlayer model id

        Nothing ->
            addPlayer model


addPlayer : Model -> Model
addPlayer model =
    let
        newPlayer =
            Player (List.length model.players) model.name 0

        newPlayers =
            newPlayer :: model.players
    in
        { model
            | players = newPlayers
            , name = " "
        }


editPlayer : Model -> Int -> Model
editPlayer model id =
    let
        newPlayers =
            List.map
                (\player ->
                    if player.id == id then
                        { player | name = model.name }
                    else
                        player
                )
                model.players

        newPlays =
            List.map
                (\play ->
                    if play.playerId == id then
                        { play | name = model.name }
                    else
                        play
                )
                model.plays
    in
        { model
            | players = newPlayers
            , plays = newPlays
            , name = ""
            , playerId = Nothing
        }



--view


view : Model -> Html Msg
view model =
    div [ class "scoreboard" ]
        [ h1 [] [ text "Score Keeper" ]
        , playerSection model
        , playerForm model
        , playSection model
        , p [] [ text <| toString model ]
        ]


playerForm : Model -> Html Msg
playerForm model =
    Html.form [ onSubmit Save ]
        [ input
            [ type_ "text"
            , placeholder "Add/Edit player.."
            , onInput Input
            , value model.name
            ]
            []
        , button [ type_ "submit" ] [ text "Save" ]
        , button [ type_ "button", onClick Cancel ] [ text "Cancel" ]
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
    model.players
        |> List.sortBy .name
        |> List.map playerToHtml
        |> ul []


playerToHtml : Player -> Html Msg
playerToHtml player =
    li []
        [ i
            [ class "edit"
            , onClick (Edit player)
            ]
            []
        , div []
            [ text player.name ]
        , button
            [ type_ "button"
            , onClick (Score player 2)
            ]
            [ text "2 pts" ]
        , button
            [ type_ "button"
            , onClick (Score player 3)
            ]
            [ text "3 pts" ]
        , div []
            [ text <| toString player.points ]
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
            , div [] [ text <| toString total ]
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
        |> List.map playToHtml
        |> ul []


playToHtml : Play -> Html Msg
playToHtml play =
    li []
        [ i
            [ class "remove"
            , onClick (DeletePlay play)
            ]
            []
        , div []
            [ text play.name ]
        , div []
            [ text <| toString play.points ]
        ]


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = initModel
        , view = view
        , update = update
        }
