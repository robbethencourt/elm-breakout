module Main exposing (..)

import Html exposing (Html, text, div, img)
import Html.Attributes exposing (class)
import Svg exposing (Svg, rect)
import Svg.Attributes exposing (width, height, viewBox, fill, x, y)
import AnimationFrame
import Time exposing (Time)
import Keyboard.Extra exposing (Key)
import Pieces.Block as Block
import Pieces.Paddle as Paddle


---- MODEL ----


type alias Model =
    { gameState : GameState
    , playerStats : Player
    , ballPosition : ( Float, Float )
    , ballVelocity : ( Float, Float )
    , paddle : Paddle.Paddle
    , paddlePosition : Int
    , blocks : List Block.Block
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


initialVelocity : Float
initialVelocity =
    0.03


init : ( Model, Cmd Msg )
init =
    ( { gameState = NotPlaying
      , playerStats = Player 0 0 0
      , ballPosition = ( -5, -5 )
      , ballVelocity = ( initialVelocity, initialVelocity )
      , paddle = Paddle.initialPaddle
      , paddlePosition = 40
      , blocks = Block.initialBlocks
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
                        , ballPosition = ( 30, 60 )
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

        -- debugVelocityx =
        --     Debug.log "ball position x" ballPositionX
        --
        -- debugVelocityY =
        --     Debug.log "ball position y" ballPositionY
        roundedBallPositionX =
            floor ballPositionX

        -- debugthree =
        --     Debug.log "rounded x" roundedBallPositionX
        --
        -- debugpaddlepos =
        --     Debug.log "paddle position" model.paddlePosition
        roundedBallPositionY =
            floor ballPositionY

        -- debugtwo =
        --     Debug.log "rounded y" roundedBallPositionY
        --
        -- debugmore =
        --     Debug.log "the bool" (List.member roundedBallPositionY (List.range model.paddlePosition (model.paddlePosition + 20)))
        ( newBallPositionX, newBallVelocityX ) =
            if roundedBallPositionY == 100 then
                ( 30, initialVelocity )
            else if ballPositionX > 99 then
                ( 98, -1 * abs ballVelocityX )
            else if ballPositionX < 0 then
                ( 0.5, abs ballVelocityX )
            else
                ( ballPositionX, ballVelocityX )

        ( newBallPositionY, newBallVelocityY, newListOfBlocks ) =
            case roundedBallPositionY of
                100 ->
                    ( 60, initialVelocity, model.blocks )

                88 ->
                    -- need the paddle position so the ball can change it's tragectory
                    if List.member roundedBallPositionX (List.range model.paddlePosition (model.paddlePosition + 20)) then
                        ( 87, -1 * abs ballVelocityY, model.blocks )
                    else
                        ( ballPositionY, ballVelocityY, model.blocks )

                21 ->
                    if not (checkBlockCollision ballPositionX 20 model) then
                        ( 22, abs ballVelocityY, updatedListOfBlocks ballPositionX 20 model.blocks )
                    else
                        ( ballPositionY, ballVelocityY, model.blocks )

                19 ->
                    if not (checkBlockCollision ballPositionX 18 model) then
                        ( 20, abs ballVelocityY, updatedListOfBlocks ballPositionX 18 model.blocks )
                    else
                        ( ballPositionY, ballVelocityY, model.blocks )

                17 ->
                    if not (checkBlockCollision ballPositionX 16 model) then
                        ( 18, abs ballVelocityY, updatedListOfBlocks ballPositionX 16 model.blocks )
                    else
                        ( ballPositionY, ballVelocityY, model.blocks )

                15 ->
                    if not (checkBlockCollision ballPositionX 14 model) then
                        ( 16, abs ballVelocityY, updatedListOfBlocks ballPositionX 14 model.blocks )
                    else
                        ( ballPositionY, ballVelocityY, model.blocks )

                13 ->
                    if not (checkBlockCollision ballPositionX 12 model) then
                        ( 14, abs ballVelocityY, updatedListOfBlocks ballPositionX 12 model.blocks )
                    else
                        ( ballPositionY, ballVelocityY, model.blocks )

                11 ->
                    if not (checkBlockCollision ballPositionX 10 model) then
                        ( 12, abs ballVelocityY, updatedListOfBlocks ballPositionX 10 model.blocks )
                    else
                        ( ballPositionY, ballVelocityY, model.blocks )

                0 ->
                    ( 0.5, abs ballVelocityY, model.blocks )

                _ ->
                    ( ballPositionY, ballVelocityY, model.blocks )
    in
        ( { model
            | ballPosition =
                ( newBallPositionX + newBallVelocityX * dt
                , newBallPositionY + newBallVelocityY * dt
                )
            , ballVelocity = ( newBallVelocityX, newBallVelocityY )
            , blocks = newListOfBlocks
          }
        , Cmd.none
        )


checkBlockCollision : Float -> Int -> Model -> Bool
checkBlockCollision ballX ballY model =
    Block.getRowOfBlocks ballY model.blocks
        |> List.filter (\block -> (List.member (round ballX) (List.range block.xPosition (block.xPosition + 5))))
        |> List.isEmpty


updatedListOfBlocks : Float -> Int -> List Block.Block -> List Block.Block
updatedListOfBlocks ballX ballY blocks =
    let
        filterBlocks bx by b =
            if List.member (round bx) (List.range b.xPosition (b.xPosition + 5)) then
                if b.yPosition == by then
                    False
                else
                    True
            else
                True
    in
        blocks
            |> List.filter (filterBlocks ballX ballY)



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
        ((Block.rowOfSvgBlocks model.blocks)
            ++ [ ball model.ballPosition ]
            ++ [ Paddle.renderPaddle model.paddlePosition ]
        )


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
