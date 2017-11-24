module Main exposing (..)

import Html exposing (Html, text, div, img)
import Html.Attributes exposing (class)


---- MODEL ----


type alias Model =
    { gameState : GameState
    , playerStats : Player
    }


type GameState
    = NotPlaying
    | Playing
    | GameOver


type alias Player =
    { score : Int
    , lives : Int
    , playerNumber : Int
    }


init : ( Model, Cmd Msg )
init =
    ( { gameState = NotPlaying
      , playerStats = Player 0 0 0
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ gameHeader model.playerStats
        , div [ class "game-container" ]
            [ case model.gameState of
                NotPlaying ->
                    text "not playing"

                Playing ->
                    text "playing"

                GameOver ->
                    text "game over"
            ]
        , div [ class "game-footer" ] []
        ]


gameHeader : Player -> Html Msg
gameHeader { score, lives, playerNumber } =
    div [ class "game-header" ]
        [ div [ class "score" ] [ text (toString score) ]
        , div [ class "lives" ] [ text (toString lives) ]
        , div [ class "player-number" ] [ text (toString playerNumber) ]
        ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        }
