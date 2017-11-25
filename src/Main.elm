module Main exposing (..)

import Html exposing (Html, text, div, img)
import Html.Attributes exposing (class)
import Svg exposing (Svg, rect)
import Svg.Attributes exposing (width, height, viewBox, fill, x, y)
import AnimationFrame
import Time exposing (Time)
import Keyboard.Extra exposing (Key)


---- MODEL ----


type alias Model =
    { gameState : GameState
    , playerStats : Player
    , ballPosition : ( Float, Float )
    , ballVelocity : ( Float, Float )
    , paddlePosition : Int
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
      , ballPosition = ( -5, -5 )
      , ballVelocity = ( 0.01, 0.01 )
      , paddlePosition = 40
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = TimeUpdate Time
    | KeyboardUpdate Key


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TimeUpdate dt ->
            case model.gameState of
                NotPlaying ->
                    ( model, Cmd.none )

                Playing ->
                    updateBallPosition dt model

                GameOver ->
                    ( { model | ballPosition = ( -5, -5 ) }, Cmd.none )

        KeyboardUpdate keycode ->
            case keycode of
                Keyboard.Extra.Space ->
                    ( { model
                        | gameState = Playing
                        , ballPosition = ( 60, 60 )
                      }
                    , Cmd.none
                    )

                Keyboard.Extra.ArrowLeft ->
                    case model.gameState of
                        Playing ->
                            if model.paddlePosition > 0 then
                                ( { model | paddlePosition = model.paddlePosition - 1 }, Cmd.none )
                            else
                                ( model, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                Keyboard.Extra.ArrowRight ->
                    case model.gameState of
                        Playing ->
                            if model.paddlePosition < 80 then
                                ( { model | paddlePosition = model.paddlePosition + 1 }, Cmd.none )
                            else
                                ( model, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )


updateBallPosition : Time -> Model -> ( Model, Cmd Msg )
updateBallPosition dt model =
    let
        ( ballPositionX, ballPositionY ) =
            model.ballPosition

        ( ballVelocityX, ballVelocityY ) =
            model.ballVelocity

        debugVelocityx =
            Debug.log "ball position x" ballPositionX

        debugVelocityY =
            Debug.log "ball position y" ballPositionY

        roundedBallPositionX =
            floor ballPositionX

        debugthree =
            Debug.log "rounded x" roundedBallPositionX

        debugpaddlepos =
            Debug.log "paddle position" model.paddlePosition

        roundedBallPositionY =
            floor ballPositionY

        debugtwo =
            Debug.log "rounded y" roundedBallPositionY

        debugmore =
            Debug.log "the bool" (model.paddlePosition >= roundedBallPositionX && model.paddlePosition <= (roundedBallPositionX + 20))

        ( newBallPositionX, newBallVelocityX ) =
            if roundedBallPositionY == 100 then
                ( 60, 0.01 )
                -- 98 ->
                --     if model.paddlePosition >= roundedBallPositionX && model.paddlePosition <= roundedBallPositionX + 20 then
                --         ( 97, -1 * abs ballVelocityX )
                --     else
                --         ( ballPositionX, ballVelocityX )
            else
                ( ballPositionX, ballVelocityX )

        ( newBallPositionY, newBallVelocityY ) =
            case roundedBallPositionY of
                100 ->
                    ( 60, 0.01 )

                88 ->
                    if List.member roundedBallPositionY (List.range model.paddlePosition (model.paddlePosition + 20)) then
                        ( 87, -1 * abs ballVelocityY )
                    else
                        ( ballPositionY, ballVelocityY )

                _ ->
                    ( ballPositionY, ballVelocityY )
    in
        ( { model
            | ballPosition =
                ( newBallPositionX + newBallVelocityX * dt
                , newBallPositionY + newBallVelocityY * dt
                )
            , ballVelocity = ( newBallVelocityX, newBallVelocityY )
          }
        , Cmd.none
        )



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ gameHeader model.playerStats
        , div [ class "game-container" ]
            [ gameBoard model ]
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


gameBoard : Model -> Html Msg
gameBoard model =
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
            ++ [ ball model.ballPosition ]
            ++ [ paddle model.paddlePosition ]
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


ball : ( Float, Float ) -> Html Msg
ball ( xPosition, yPosition ) =
    rect
        [ width "1.5"
        , height "1.5"
        , fill "#C64947"
        , x (toString xPosition)
        , y (toString yPosition)
        ]
        []


paddle : Int -> Html Msg
paddle xPosition =
    rect
        [ width "20"
        , height "1.5"
        , fill "#C64947"
        , x (toString xPosition)
        , y "88.5"
        ]
        []



---- PROGRAM ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs TimeUpdate
        , Keyboard.Extra.downs KeyboardUpdate
        ]


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
