module Main exposing (..)

import Html exposing (Html, text, div, img, p)
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
    GameState


type GameState
    = NotPlaying GameModel
    | Playing GameModel
    | Won GameModel
    | Lost GameModel


type alias GameModel =
    { player : Player
    , ballPosition : ( Float, Float )
    , ballVelocity : ( Float, Float )
    , paddle : Paddle.Paddle
    , paddlePosition : Float
    , gameControls : GameControls
    , currentCollision : Collision
    , gameSpeed : Speed
    , blocks : List Block.Block
    }


type alias Player =
    { score : Int
    , lives : Int
    , playerNumber : Int
    }


type BallMovement
    = NotMoving
    | Moving


type alias GameControls =
    { paddleLeft : Bool
    , paddleRight : Bool
    , ballMovement : BallMovement
    }


type Speed
    = Slow
    | Fast


initGameModel : GameModel
initGameModel =
    { player = initPlayer
    , ballPosition = ( 19, initBallPosition )
    , ballVelocity = ( initVelocity, initVelocity )
    , paddle = Paddle.normalPaddle
    , paddlePosition = 40
    , gameControls = initGameControls
    , currentCollision = BottomWallCollision
    , gameSpeed = Slow
    , blocks = Block.initBlocks
    }


initPlayer : Player
initPlayer =
    Player 0 5 1


initBallPosition : Float
initBallPosition =
    73


initVelocity : Float
initVelocity =
    -0.04


initGameControls : GameControls
initGameControls =
    { paddleLeft = False
    , paddleRight = False
    , ballMovement = NotMoving
    }


init : ( Model, Cmd Msg )
init =
    ( NotPlaying initGameModel, Cmd.none )



---- UPDATE ----


type Msg
    = TimeUpdate Time
    | GameInput GameControl Bool
    | NoOp


type GameControl
    = PaddleLeft
    | PaddleRight
    | LaunchBall
    | ResetGame


keyToGameControl : Bool -> Key -> Msg
keyToGameControl isActive key =
    case key of
        Keyboard.Extra.ArrowLeft ->
            GameInput PaddleLeft isActive

        Keyboard.Extra.ArrowRight ->
            GameInput PaddleRight isActive

        Keyboard.Extra.Space ->
            GameInput LaunchBall isActive

        Keyboard.Extra.Enter ->
            GameInput ResetGame isActive

        _ ->
            NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TimeUpdate dt ->
            case model of
                NotPlaying _ ->
                    ( model, Cmd.none )

                Playing gameModel ->
                    updateGameDisplay dt gameModel

                Won _ ->
                    ( model, Cmd.none )

                Lost _ ->
                    ( model, Cmd.none )

        GameInput gameControl isActive ->
            case model of
                NotPlaying gameState ->
                    case gameControl of
                        LaunchBall ->
                            let
                                gameControls =
                                    gameState.gameControls

                                updatedGameControls =
                                    { gameControls | ballMovement = Moving }

                                updatedGameState =
                                    { gameState | gameControls = updatedGameControls }
                            in
                                ( Playing updatedGameState, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                Playing gameState ->
                    let
                        gameControls =
                            gameState.gameControls
                    in
                        case gameControl of
                            PaddleLeft ->
                                let
                                    updatedGameControls =
                                        { gameControls | paddleLeft = isActive }

                                    updatedGameState =
                                        { gameState | gameControls = updatedGameControls }
                                in
                                    ( Playing updatedGameState, Cmd.none )

                            PaddleRight ->
                                let
                                    updatedGameControls =
                                        { gameControls | paddleRight = isActive }

                                    updatedGameState =
                                        { gameState | gameControls = updatedGameControls }
                                in
                                    ( Playing updatedGameState, Cmd.none )

                            LaunchBall ->
                                let
                                    gameControls =
                                        gameState.gameControls
                                in
                                    if gameControls.ballMovement == Moving then
                                        ( model, Cmd.none )
                                    else
                                        let
                                            updatedGameControls =
                                                { gameControls | ballMovement = Moving }

                                            updatedGameState =
                                                { gameState
                                                    | gameControls = updatedGameControls
                                                    , ballVelocity = ( initVelocity, initVelocity )
                                                }
                                        in
                                            ( Playing updatedGameState, Cmd.none )

                            ResetGame ->
                                ( NotPlaying initGameModel, Cmd.none )

                Won gameModel ->
                    case gameControl of
                        ResetGame ->
                            ( NotPlaying initGameModel, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                Lost gameModel ->
                    case gameControl of
                        ResetGame ->
                            ( NotPlaying initGameModel, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



-- detect collisions


type Collision
    = TopWallCollision
    | RightWallCollision
    | BottomWallCollision
    | LeftWallCollision
    | LeftPaddleCollision
    | LeftMiddlePaddleCollision
    | MiddlePaddleCollision
    | RightMiddlePaddleCollision
    | RightPaddleCollision
    | TopBlockCollision Block.Block
    | BottomBlockCollision Block.Block


updateGameDisplay : Time -> GameModel -> ( Model, Cmd Msg )
updateGameDisplay dt gameModel =
    let
        ( ballPositionX, ballPositionY ) =
            gameModel.ballPosition

        ( ballVelocityX, ballVelocityY ) =
            gameModel.ballVelocity

        playerStats =
            gameModel.player
    in
        if playerStats.score == 480 then
            ( Won gameModel, Cmd.none )
        else if playerStats.lives == 0 then
            ( Lost gameModel, Cmd.none )
        else if ballPositionY < 73 && ballPositionY > 22 && ballPositionX > 0 && ballPositionX < 100 then
            ( Playing
                { gameModel
                    | ballPosition =
                        ( ballPositionX + ballVelocityX * dt
                        , ballPositionY + ballVelocityY * dt
                        )
                    , paddlePosition = updatePaddlePosition gameModel.gameControls gameModel.paddlePosition gameModel.paddle
                }
            , Cmd.none
            )
        else
            let
                maybeCollision =
                    collectCollisions dt gameModel
            in
                case maybeCollision of
                    Nothing ->
                        ( Playing
                            { gameModel
                                | ballPosition =
                                    ( ballPositionX + ballVelocityX * dt
                                    , ballPositionY + ballVelocityY * dt
                                    )
                                , paddlePosition = updatePaddlePosition gameModel.gameControls gameModel.paddlePosition gameModel.paddle
                            }
                        , Cmd.none
                        )

                    Just collision ->
                        let
                            gameModelWithUpdatedGameControls =
                                { gameModel | paddlePosition = updatePaddlePosition gameModel.gameControls gameModel.paddlePosition gameModel.paddle }
                        in
                            ( Playing <| handleCollision collision gameModelWithUpdatedGameControls, Cmd.none )


handleCollision : Collision -> GameModel -> GameModel
handleCollision collision gameModel =
    let
        ( ballX, ballY ) =
            gameModel.ballPosition

        ( ballVelocityX, ballVelocityY ) =
            gameModel.ballVelocity
    in
        case collision of
            TopWallCollision ->
                { gameModel
                    | ballPosition = ( ballX, 0.1 )
                    , ballVelocity = ( ballVelocityX, abs ballVelocityY )
                    , currentCollision = TopWallCollision
                }

            RightWallCollision ->
                { gameModel
                    | ballPosition = ( 99.9, ballY )
                    , ballVelocity = ( -1 * abs ballVelocityX, ballVelocityY )
                    , currentCollision = RightWallCollision
                }

            BottomWallCollision ->
                let
                    player =
                        gameModel.player

                    gameControls =
                        gameModel.gameControls

                    updatedLives =
                        if player.lives == 0 then
                            0
                        else
                            player.lives - 1
                in
                    { gameModel
                        | player = { player | lives = updatedLives }
                        , ballPosition = ( initBallPosition - toFloat (7 * updatedLives), initBallPosition )
                        , ballVelocity = ( 0, 0 )
                        , gameControls = { gameControls | ballMovement = NotMoving }
                        , currentCollision = BottomWallCollision
                        , gameSpeed = Slow
                        , paddle = Paddle.normalPaddle
                    }

            LeftWallCollision ->
                { gameModel
                    | ballPosition = ( 0.1, ballY )
                    , ballVelocity = ( abs ballVelocityX, ballVelocityY )
                    , currentCollision = LeftWallCollision
                }

            LeftPaddleCollision ->
                { gameModel
                    | ballPosition = ( ballX, 74.9 )
                    , ballVelocity = ( -1 * abs ballVelocityX, -1 * abs ballVelocityY )
                    , currentCollision = LeftPaddleCollision
                }

            LeftMiddlePaddleCollision ->
                { gameModel
                    | ballPosition = ( ballX, 74.9 )
                    , ballVelocity = ( ballVelocityX, -1 * abs ballVelocityY )
                    , currentCollision = LeftMiddlePaddleCollision
                }

            MiddlePaddleCollision ->
                { gameModel
                    | ballPosition = ( ballX, 74.9 )
                    , ballVelocity = ( ballVelocityX, -1 * abs ballVelocityY )
                    , currentCollision = MiddlePaddleCollision
                }

            RightMiddlePaddleCollision ->
                { gameModel
                    | ballPosition = ( ballX, 74.9 )
                    , ballVelocity = ( ballVelocityX, -1 * abs ballVelocityY )
                    , currentCollision = RightMiddlePaddleCollision
                }

            RightPaddleCollision ->
                { gameModel
                    | ballPosition = ( ballX, 74.9 )
                    , ballVelocity = ( abs ballVelocityX, -1 * abs ballVelocityY )
                    , currentCollision = RightPaddleCollision
                }

            TopBlockCollision block ->
                let
                    ( updatedBallVelocityX, updatedBallVelocityY, updatedGameSpeed, updatedPaddle ) =
                        if block.yPosition < 14 && gameModel.gameSpeed == Slow then
                            ( ballVelocityX * 1.85, ballVelocityY * 1.85, Fast, Paddle.shortPaddle )
                        else
                            ( ballVelocityX, ballVelocityY, gameModel.gameSpeed, gameModel.paddle )

                    player =
                        gameModel.player
                in
                    { gameModel
                        | player = { player | score = player.score + block.value }
                        , ballPosition = ( ballX, (toFloat block.yPosition) - 2 )
                        , ballVelocity = ( updatedBallVelocityX, -1 * abs updatedBallVelocityY )
                        , blocks = List.filter (\b -> b /= block) gameModel.blocks
                        , currentCollision = TopBlockCollision block
                        , gameSpeed = updatedGameSpeed
                        , paddle = updatedPaddle
                    }

            BottomBlockCollision block ->
                let
                    ( updatedBallVelocityX, updatedBallVelocityY, updatedGameSpeed, updatedPaddle ) =
                        if block.yPosition < 14 && gameModel.gameSpeed == Slow then
                            ( ballVelocityX * 1.85, ballVelocityY * 1.85, Fast, Paddle.shortPaddle )
                        else
                            ( ballVelocityX, ballVelocityY, gameModel.gameSpeed, gameModel.paddle )

                    player =
                        gameModel.player
                in
                    { gameModel
                        | player = { player | score = player.score + block.value }
                        , ballPosition = ( ballX, (toFloat block.yPosition) + 2 )
                        , ballVelocity = ( updatedBallVelocityX, abs updatedBallVelocityY )
                        , blocks = List.filter (\b -> b /= block) gameModel.blocks
                        , currentCollision = BottomBlockCollision block
                        , gameSpeed = updatedGameSpeed
                        , paddle = updatedPaddle
                    }


collectCollisions : Time -> GameModel -> Maybe Collision
collectCollisions dt gameModel =
    detectCollisions dt gameModel
        |> List.filterMap identity
        |> List.head


detectCollisions : Time -> GameModel -> List (Maybe Collision)
detectCollisions dt gameModel =
    let
        wallCollision =
            detectWallCollision gameModel.ballPosition

        paddleCollision =
            detectPaddleCollision gameModel.ballPosition gameModel.paddlePosition gameModel.paddle

        blockCollision =
            detectBlockCollision gameModel.currentCollision gameModel.ballPosition gameModel.ballVelocity gameModel.blocks
    in
        [ wallCollision, paddleCollision, blockCollision ]


detectWallCollision : ( Float, Float ) -> Maybe Collision
detectWallCollision ( ballPosX, ballPosY ) =
    if ballPosY > 100 then
        Just BottomWallCollision
    else if ballPosY < 0 then
        Just TopWallCollision
    else if ballPosX > 100 then
        Just RightWallCollision
    else if ballPosX < 0 then
        Just LeftWallCollision
    else
        Nothing


detectPaddleCollision : ( Float, Float ) -> Float -> Paddle.Paddle -> Maybe Collision
detectPaddleCollision ( ballPosX, ballPosY ) paddlePosition paddle =
    if ballPosY >= 75 && ballPosY < 77 then
        case paddle of
            Paddle.NormalPaddle paddleSections ->
                if ballPosX < paddlePosition || ballPosX > paddlePosition + 20 then
                    Nothing
                else
                    let
                        ballAlongPaddle =
                            floor (ballPosX - paddlePosition)
                    in
                        case ballAlongPaddle - (ballAlongPaddle % 4) of
                            0 ->
                                Just LeftPaddleCollision

                            4 ->
                                Just LeftMiddlePaddleCollision

                            8 ->
                                Just MiddlePaddleCollision

                            12 ->
                                Just RightMiddlePaddleCollision

                            16 ->
                                Just RightPaddleCollision

                            _ ->
                                Nothing

            Paddle.ShortPaddle paddleSections ->
                if ballPosX < paddlePosition || ballPosX > paddlePosition + 10 then
                    Nothing
                else
                    let
                        ballAlongPaddle =
                            floor (ballPosX - paddlePosition)
                    in
                        case ballAlongPaddle - (ballAlongPaddle % 4) of
                            0 ->
                                Just LeftPaddleCollision

                            2 ->
                                Just LeftMiddlePaddleCollision

                            4 ->
                                Just MiddlePaddleCollision

                            6 ->
                                Just RightMiddlePaddleCollision

                            8 ->
                                Just RightPaddleCollision

                            _ ->
                                Nothing
    else
        Nothing


detectBlockCollision : Collision -> ( Float, Float ) -> ( Float, Float ) -> List Block.Block -> Maybe Collision
detectBlockCollision currentCollision ( ballPosX, ballPosY ) ( ballVelX, ballVelY ) blocks =
    case currentCollision of
        TopBlockCollision _ ->
            Nothing

        BottomBlockCollision _ ->
            Nothing

        _ ->
            if ballPosY < 20 || ballPosY >= 8 then
                let
                    ballYFloor =
                        floor ballPosY

                    ballYToGetRow =
                        ballYFloor - (ballYFloor % 2)

                    ballXFloor =
                        floor ballPosX

                    ballXToGetBlock =
                        ballXFloor - (ballXFloor % 5)

                    maybeBlock =
                        blocks
                            |> Block.getRowOfBlocks ballYToGetRow
                            |> Block.getBlock ballXToGetBlock
                in
                    case maybeBlock of
                        Just block ->
                            if ballVelY < 0 then
                                Just <| BottomBlockCollision block
                            else
                                Just <| TopBlockCollision block

                        Nothing ->
                            Nothing
            else
                Nothing


updatePaddlePosition : GameControls -> Float -> Paddle.Paddle -> Float
updatePaddlePosition gameControls paddlePosition paddle =
    if gameControls.paddleLeft then
        if paddlePosition > 0 then
            paddlePosition - 1.35
        else
            paddlePosition
    else if gameControls.paddleRight then
        case paddle of
            Paddle.NormalPaddle paddleSize ->
                if paddlePosition < 80 then
                    paddlePosition + 1.35
                else
                    paddlePosition

            Paddle.ShortPaddle paddleSize ->
                if paddlePosition < 90 then
                    paddlePosition + 1.35
                else
                    paddlePosition
    else
        paddlePosition



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ gameHeader model
        , div [ class "game-container" ]
            [ gameBoard model
            , case model of
                NotPlaying gameState ->
                    div [ class "game-content" ]
                        [ p [] [ text "Press enter to reset game, spacebar to launch ball, left and right arrows to move paddle." ]
                        , p [] [ text "Have Fun!" ]
                        ]

                Playing gameState ->
                    div [] []

                Won gameState ->
                    div [ class "game-content" ]
                        [ p [] [ text "Congratulations" ] ]

                Lost gameState ->
                    div [ class "game-content" ]
                        [ p [] [ text "Try Again. Press Enter to reset game!" ] ]
            ]
        , div [ class "game-footer" ] []
        ]


gameHeader : Model -> Html Msg
gameHeader model =
    case model of
        NotPlaying gameModel ->
            displayScore gameModel.player

        Playing gameModel ->
            displayScore gameModel.player

        Won gameModel ->
            displayScore gameModel.player

        Lost gameModel ->
            displayScore gameModel.player


displayScore : Player -> Html Msg
displayScore { score, lives, playerNumber } =
    div [ class "game-header" ]
        [ div [ class "score" ] [ text (toString score) ]
        , div [ class "lives" ] [ text (toString lives) ]
        , div [ class "player-number" ] [ text (toString playerNumber) ]
        ]


gameBoard : Model -> Html Msg
gameBoard model =
    case model of
        NotPlaying gameModel ->
            displayGameBoard gameModel

        Playing gameModel ->
            displayGameBoard gameModel

        Won gameModel ->
            displayGameBoard gameModel

        Lost gameModel ->
            displayGameBoard gameModel


displayGameBoard : GameModel -> Html Msg
displayGameBoard gameModel =
    Svg.svg
        [ width "100%"
        , height "100%"
        , viewBox "0 0 100 77"
        , fill "#000000"
        ]
        ((Block.rowOfSvgBlocks gameModel.blocks)
            ++ [ Ball.ball gameModel.ballPosition ]
            ++ [ Paddle.renderPaddle gameModel.paddlePosition gameModel.paddle ]
        )



---- PROGRAM ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs TimeUpdate
        , Keyboard.Extra.downs <| keyToGameControl True
        , Keyboard.Extra.ups <| keyToGameControl False
        ]


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
