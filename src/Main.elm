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
import Pieces.Ball as Ball


---- MODEL ----


type alias Model =
    { gameState : GameState
    , playerStats : Player
    , ballPosition : ( Float, Float )
    , ballVelocity : ( Float, Float )
    , ballDirection : Ball.BallDirection
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
      , playerStats =
            Player 0 0 0
            -- , ballPosition = ( -5, -5 )
      , ballPosition = ( 40, 80 )
      , ballVelocity = ( initialVelocity, initialVelocity )
      , ballDirection = Ball.Down
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
                        | gameState =
                            Playing
                            -- , ballPosition = ( 3, 6 )
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

        debugBallDirection =
            Debug.log "ball direction" model.ballDirection

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

        ( newBallPositionY, newBallVelocityY, newListOfBlocks, newBallDirection ) =
            -- newListOfBlocks should be a maybe block to remove so i can use the same logic above
            case roundedBallPositionY of
                100 ->
                    ( 60, initialVelocity, model.blocks, Ball.Down )

                88 ->
                    -- need the paddle position so the ball can change it's tragectory
                    if List.member roundedBallPositionX (List.range model.paddlePosition (model.paddlePosition + 20)) then
                        ( 87, -1 * abs ballVelocityY, model.blocks, Ball.reverseBallDirection model.ballDirection )
                    else
                        ( ballPositionY, ballVelocityY, model.blocks, model.ballDirection )

                22 ->
                    case model.ballDirection of
                        Ball.Up ->
                            let
                                theBlockToRemoveInAList =
                                    checkBlockCollision ballPositionX 20 model

                                theBlockToRemove =
                                    theBlockToRemoveInAList
                                        |> List.head
                                        |> Maybe.withDefault (Block.Block "" 0 0 0)
                            in
                                if List.length theBlockToRemoveInAList > 0 then
                                    ( 23, abs ballVelocityY, updateListOfBlocks theBlockToRemove model.blocks, Ball.reverseBallDirection model.ballDirection )
                                else
                                    ( ballPositionY, ballVelocityY, model.blocks, model.ballDirection )

                        Ball.Down ->
                            ( ballPositionY, ballVelocityY, model.blocks, model.ballDirection )

                20 ->
                    case model.ballDirection of
                        Ball.Up ->
                            let
                                theBlockToRemoveInAList =
                                    checkBlockCollision ballPositionX 18 model

                                theBlockToRemove =
                                    theBlockToRemoveInAList
                                        |> List.head
                                        |> Maybe.withDefault (Block.Block "" 0 0 0)
                            in
                                if List.length theBlockToRemoveInAList > 0 then
                                    ( 21, -1 * abs ballVelocityY, updateListOfBlocks theBlockToRemove model.blocks, Ball.reverseBallDirection model.ballDirection )
                                else
                                    ( ballPositionY, ballVelocityY, model.blocks, model.ballDirection )

                        Ball.Down ->
                            let
                                theBlockToRemoveInAList =
                                    checkBlockCollision ballPositionX 20 model

                                theBlockToRemove =
                                    theBlockToRemoveInAList
                                        |> List.head
                                        |> Maybe.withDefault (Block.Block "" 0 0 0)
                            in
                                if List.length theBlockToRemoveInAList > 0 then
                                    ( 19, abs ballVelocityY, updateListOfBlocks theBlockToRemove model.blocks, Ball.reverseBallDirection model.ballDirection )
                                else
                                    ( ballPositionY, ballVelocityY, model.blocks, model.ballDirection )

                18 ->
                    case model.ballDirection of
                        Ball.Up ->
                            let
                                theBlockToRemoveInAList =
                                    checkBlockCollision ballPositionX 16 model

                                theBlockToRemove =
                                    theBlockToRemoveInAList
                                        |> List.head
                                        |> Maybe.withDefault (Block.Block "" 0 0 0)
                            in
                                if List.length theBlockToRemoveInAList > 0 then
                                    ( 19, abs ballVelocityY, updateListOfBlocks theBlockToRemove model.blocks, Ball.reverseBallDirection model.ballDirection )
                                else
                                    ( ballPositionY, ballVelocityY, model.blocks, model.ballDirection )

                        Ball.Down ->
                            let
                                theBlockToRemoveInAList =
                                    checkBlockCollision ballPositionX 18 model

                                theBlockToRemove =
                                    theBlockToRemoveInAList
                                        |> List.head
                                        |> Maybe.withDefault (Block.Block "" 0 0 0)
                            in
                                if List.length theBlockToRemoveInAList > 0 then
                                    ( 17, -1 * abs ballVelocityY, updateListOfBlocks theBlockToRemove model.blocks, Ball.reverseBallDirection model.ballDirection )
                                else
                                    ( ballPositionY, ballVelocityY, model.blocks, model.ballDirection )

                16 ->
                    case model.ballDirection of
                        Ball.Up ->
                            let
                                theBlockToRemoveInAList =
                                    checkBlockCollision ballPositionX 14 model

                                theBlockToRemove =
                                    theBlockToRemoveInAList
                                        |> List.head
                                        |> Maybe.withDefault (Block.Block "" 0 0 0)
                            in
                                if List.length theBlockToRemoveInAList > 0 then
                                    ( 17, abs ballVelocityY, updateListOfBlocks theBlockToRemove model.blocks, Ball.reverseBallDirection model.ballDirection )
                                else
                                    ( ballPositionY, ballVelocityY, model.blocks, model.ballDirection )

                        Ball.Down ->
                            let
                                theBlockToRemoveInAList =
                                    checkBlockCollision ballPositionX 16 model

                                theBlockToRemove =
                                    theBlockToRemoveInAList
                                        |> List.head
                                        |> Maybe.withDefault (Block.Block "" 0 0 0)
                            in
                                if List.length theBlockToRemoveInAList > 0 then
                                    ( 15, -1 * abs ballVelocityY, updateListOfBlocks theBlockToRemove model.blocks, Ball.reverseBallDirection model.ballDirection )
                                else
                                    ( ballPositionY, ballVelocityY, model.blocks, model.ballDirection )

                14 ->
                    case model.ballDirection of
                        Ball.Up ->
                            let
                                theBlockToRemoveInAList =
                                    checkBlockCollision ballPositionX 12 model

                                theBlockToRemove =
                                    theBlockToRemoveInAList
                                        |> List.head
                                        |> Maybe.withDefault (Block.Block "" 0 0 0)
                            in
                                if List.length theBlockToRemoveInAList > 0 then
                                    ( 15, abs ballVelocityY, updateListOfBlocks theBlockToRemove model.blocks, Ball.reverseBallDirection model.ballDirection )
                                else
                                    ( ballPositionY, ballVelocityY, model.blocks, model.ballDirection )

                        Ball.Down ->
                            let
                                theBlockToRemoveInAList =
                                    checkBlockCollision ballPositionX 14 model

                                theBlockToRemove =
                                    theBlockToRemoveInAList
                                        |> List.head
                                        |> Maybe.withDefault (Block.Block "" 0 0 0)
                            in
                                if List.length theBlockToRemoveInAList > 0 then
                                    ( 13, -1 * abs ballVelocityY, updateListOfBlocks theBlockToRemove model.blocks, Ball.reverseBallDirection model.ballDirection )
                                else
                                    ( ballPositionY, ballVelocityY, model.blocks, model.ballDirection )

                12 ->
                    case model.ballDirection of
                        Ball.Up ->
                            let
                                theBlockToRemoveInAList =
                                    checkBlockCollision ballPositionX 10 model

                                theBlockToRemove =
                                    theBlockToRemoveInAList
                                        |> List.head
                                        |> Maybe.withDefault (Block.Block "" 0 0 0)
                            in
                                if List.length theBlockToRemoveInAList > 0 then
                                    ( 13, abs ballVelocityY, updateListOfBlocks theBlockToRemove model.blocks, Ball.reverseBallDirection model.ballDirection )
                                else
                                    ( ballPositionY, ballVelocityY, model.blocks, model.ballDirection )

                        Ball.Down ->
                            let
                                theBlockToRemoveInAList =
                                    checkBlockCollision ballPositionX 12 model

                                theBlockToRemove =
                                    theBlockToRemoveInAList
                                        |> List.head
                                        |> Maybe.withDefault (Block.Block "" 0 0 0)
                            in
                                if List.length theBlockToRemoveInAList > 0 then
                                    ( 11, -1 * abs ballVelocityY, updateListOfBlocks theBlockToRemove model.blocks, Ball.reverseBallDirection model.ballDirection )
                                else
                                    ( ballPositionY, ballVelocityY, model.blocks, model.ballDirection )

                10 ->
                    case model.ballDirection of
                        Ball.Up ->
                            ( ballPositionY, ballVelocityY, model.blocks, model.ballDirection )

                        Ball.Down ->
                            let
                                theBlockToRemoveInAList =
                                    checkBlockCollision ballPositionX 10 model

                                theBlockToRemove =
                                    theBlockToRemoveInAList
                                        |> List.head
                                        |> Maybe.withDefault (Block.Block "" 0 0 0)
                            in
                                if List.length theBlockToRemoveInAList > 0 then
                                    ( 9, -1 * abs ballVelocityY, updateListOfBlocks theBlockToRemove model.blocks, Ball.reverseBallDirection model.ballDirection )
                                else
                                    ( ballPositionY, ballVelocityY, model.blocks, model.ballDirection )

                0 ->
                    ( 1, abs ballVelocityY, model.blocks, Ball.reverseBallDirection model.ballDirection )

                _ ->
                    ( ballPositionY, ballVelocityY, model.blocks, model.ballDirection )
    in
        ( { model
            | ballPosition =
                ( newBallPositionX + newBallVelocityX * dt
                , newBallPositionY + newBallVelocityY * dt
                )
            , ballVelocity = ( newBallVelocityX, newBallVelocityY )
            , ballDirection = newBallDirection
            , blocks = newListOfBlocks
          }
        , Cmd.none
        )


checkBlockCollision : Float -> Int -> Model -> List Block.Block
checkBlockCollision ballX ballY model =
    Block.getRowOfBlocks ballY model.blocks
        -- |> List.filter (\block -> (List.member (round ballX) (List.range block.xPosition (block.xPosition + 5))))
        |>
            List.filter (\block -> ballX >= (toFloat block.xPosition) && ballX <= (toFloat (block.xPosition + 5)))
        |> Debug.log "how many lists of blocks"



-- |> List.isEmpty


updateListOfBlocks : Block.Block -> List Block.Block -> List Block.Block
updateListOfBlocks b blocks =
    -- let
    --     debugthis =
    --         Debug.log "round bx" (round ballX)
    --
    --     theBlockToRemove =
    --         List.filter (\block -> (List.member (round ballX) (List.range block.xPosition (block.xPosition + 5)))) (Block.getRowOfBlocks ballY blocks)
    --             |> List.head
    --             |> Maybe.withDefault (Block.Block "" 0 0 0)
    --
    --     debugthistoo =
    --         Debug.log "the one to remove" theBlockToRemove
    --
    --     filterBlocks blockToRemove by blockToCheck =
    --         let
    --             debugmore =
    --                 Debug.log "btr" (blockToRemove == blockToCheck)
    --         in
    --             if blockToRemove == blockToCheck then
    --                 if blockToCheck.yPosition == by then
    --                     False
    --                 else
    --                     True
    --             else
    --                 True
    -- in
    List.filter (\block -> block /= b) blocks



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
            ++ [ Ball.ball model.ballPosition ]
            ++ [ Paddle.renderPaddle model.paddlePosition ]
        )



---- PROGRAM ----


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.gameState of
        Playing ->
            Sub.batch
                [ AnimationFrame.diffs TimeUpdate
                , Keyboard.Extra.downs KeyboardUpdate
                ]

        _ ->
            Keyboard.Extra.downs KeyboardUpdate


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
