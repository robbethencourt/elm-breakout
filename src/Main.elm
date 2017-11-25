module Main exposing (..)

import Html exposing (Html, text, div, img)
import Html.Attributes exposing (class)
import Svg exposing (Svg, rect)
import Svg.Attributes exposing (width, height, viewBox, fill, x, y)


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
            [ gameBoard ]
          -- [ case model.gameState of
          --     NotPlaying ->
          --         text "not playing"
          --
          --     Playing ->
          --         text "playing"
          --
          --     GameOver ->
          --         text "game over"
          -- ]
        , div [ class "game-footer" ] []
        ]


gameHeader : Player -> Html Msg
gameHeader { score, lives, playerNumber } =
    div [ class "game-header" ]
        [ div [ class "score" ] [ text (toString score) ]
        , div [ class "lives" ] [ text (toString lives) ]
        , div [ class "player-number" ] [ text (toString playerNumber) ]
        ]


gameBoard : Html Msg
gameBoard =
    Svg.svg
        [ width "100%"
        , height "100%"
        , viewBox "0 0 100 90"
        , fill "#000000"
        ]
        ((rowOfBlocks "#CB4744" "10")
            ++ (rowOfBlocks "#C76C3A" "12")
            ++ (rowOfBlocks "#B47830" "14")
            ++ (rowOfBlocks "#9FA426" "16")
            ++ (rowOfBlocks "#46A047" "18")
            ++ (rowOfBlocks "#4546C9" "20")
            ++ [ ball ]
            ++ [ paddle ]
        )


rowOfBlocks : String -> String -> List (Html Msg)
rowOfBlocks fillColor columnPosition =
    List.map (block fillColor columnPosition) (List.range 0 19)


block : String -> String -> Int -> Html Msg
block fillColor yPosition xPosition =
    rect
        [ width "5%"
        , height "2"
        , fill fillColor
        , x (toString (xPosition * 5) ++ "%")
        , y yPosition
        ]
        []


ball : Html Msg
ball =
    rect
        [ width "1.5"
        , height "1.5"
        , fill "#C64947"
        , x "60"
        , y "60"
        ]
        []


paddle : Html Msg
paddle =
    rect
        [ width "20"
        , height "1.5"
        , fill "#C64947"
        , x "40"
        , y "88.5"
        ]
        []



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        }
