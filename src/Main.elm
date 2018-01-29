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
    , gameControls : GameControls
    , paddlePosition : Float
    , currentCollision : Collision
    , gameSpeed : Speed
    , blocks : List Block.Block
    }


type GameState
    = NotPlaying
    | Playing
    | Won
    | Lost


type alias Player =
    { score : Int
    , lives : Int
    , playerNumber : Int
    }


type Speed
    = Slow
    | Fast


type alias GameControls =
    { paddleLeft : Bool
    , paddleRight : Bool
    , launchBall : Bool
    }


initialGameControls : GameControls
initialGameControls =
    { paddleLeft = False
    , paddleRight = False
    , launchBall = False
    }


initialVelocity : Float
initialVelocity =
    0.03


init : ( Model, Cmd Msg )
init =
    ( { gameState = NotPlaying
      , playerStats = Player 0 0 0
      , ballPosition = ( 40, 60 )
      , ballVelocity = ( initialVelocity, initialVelocity )
      , ballDirection = Ball.Down
      , paddle = Paddle.normalPaddle
      , gameControls = initialGameControls
      , paddlePosition = 40
      , currentCollision = BottomWallCollision
      , gameSpeed = Slow
      , blocks = Block.initialBlocks
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = TimeUpdate Time
    | KeyboardPress Key
    | GameInput GameControl Bool
    | NoOp


type GameControl
    = PaddleLeft
    | PaddleRight


keyToGameControl : Bool -> Key -> Msg
keyToGameControl isActive key =
    case key of
        Keyboard.Extra.ArrowLeft ->
            GameInput PaddleLeft isActive

        Keyboard.Extra.ArrowRight ->
            GameInput PaddleRight isActive

        _ ->
            NoOp



-- consider KeyboardInput GameControl isActive / the Action type needs to convert keycodes to types to pass to the KeybaordInput


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TimeUpdate dt ->
            case model.gameState of
                NotPlaying ->
                    ( model, Cmd.none )

                Playing ->
                    updateBallPosition2 dt model

                Won ->
                    ( { model | ballPosition = ( -5, -5 ) }, Cmd.none )

                Lost ->
                    ( { model | ballPosition = ( -5, -5 ) }, Cmd.none )

        KeyboardPress keycode ->
            case keycode of
                Keyboard.Extra.Space ->
                    ( { model
                        | gameState = Playing
                      }
                    , Cmd.none
                    )

                -- Keyboard.Extra.ArrowLeft ->
                --     case model.gameState of
                --         Playing ->
                --             if model.paddlePosition > 0 then
                --                 ( { model | paddlePosition = model.paddlePosition - 1 }, Cmd.none )
                --             else
                --                 ( model, Cmd.none )
                --
                --         _ ->
                --             ( model, Cmd.none )
                --
                -- Keyboard.Extra.ArrowRight ->
                --     case model.gameState of
                --         Playing ->
                --             if model.paddlePosition < 80 then
                --                 ( { model | paddlePosition = model.paddlePosition + 1 }, Cmd.none )
                --             else
                --                 ( model, Cmd.none )
                --
                --         _ ->
                --             ( model, Cmd.none )
                _ ->
                    ( model, Cmd.none )

        GameInput gameControl isActive ->
            let
                gameControls =
                    model.gameControls
            in
                case gameControl of
                    PaddleLeft ->
                        ( { model
                            | gameControls = { gameControls | paddleLeft = isActive }
                          }
                        , Cmd.none
                        )

                    PaddleRight ->
                        ( { model
                            | gameControls = { gameControls | paddleRight = isActive }
                          }
                        , Cmd.none
                        )

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


updateBallPosition2 : Time -> Model -> ( Model, Cmd Msg )
updateBallPosition2 dt model =
    let
        ( ballPositionX, ballPositionY ) =
            model.ballPosition

        ( ballVelocityX, ballVelocityY ) =
            model.ballVelocity
    in
        if ballPositionY < 86 && ballPositionY > 24 && ballPositionX > 0 && ballPositionX < 100 then
            ( { model
                | ballPosition =
                    ( ballPositionX + ballVelocityX * dt
                    , ballPositionY + ballVelocityY * dt
                    )
                , paddlePosition = updatePaddlePosition model.gameControls model.paddlePosition model.paddle
              }
            , Cmd.none
            )
        else
            let
                maybeCollision =
                    collectCollisions dt model
            in
                case maybeCollision of
                    Nothing ->
                        ( { model
                            | ballPosition =
                                ( ballPositionX + ballVelocityX * dt
                                , ballPositionY + ballVelocityY * dt
                                )
                            , paddlePosition = updatePaddlePosition model.gameControls model.paddlePosition model.paddle
                          }
                        , Cmd.none
                        )

                    Just collision ->
                        let
                            modelWithUpdatedGameControls =
                                { model | paddlePosition = updatePaddlePosition model.gameControls model.paddlePosition model.paddle }
                        in
                            handleCollision collision modelWithUpdatedGameControls


handleCollision : Collision -> Model -> ( Model, Cmd Msg )
handleCollision collision model =
    let
        ( ballX, ballY ) =
            model.ballPosition

        ( ballVelocityX, ballVelocityY ) =
            model.ballVelocity
    in
        case collision of
            TopWallCollision ->
                ( { model
                    | ballPosition = ( ballX, 0.1 )
                    , ballVelocity = ( ballVelocityX, abs ballVelocityY )
                    , currentCollision = TopWallCollision
                  }
                , Cmd.none
                )

            RightWallCollision ->
                ( { model
                    | ballPosition = ( 99.9, ballY )
                    , ballVelocity = ( -1 * abs ballVelocityX, ballVelocityY )
                    , currentCollision = RightWallCollision
                  }
                , Cmd.none
                )

            BottomWallCollision ->
                ( { model
                    | ballPosition = ( 60, 60 )
                    , ballVelocity = ( initialVelocity, initialVelocity )
                    , currentCollision = BottomWallCollision
                    , gameSpeed = Slow
                    , paddle = Paddle.normalPaddle
                  }
                , Cmd.none
                )

            LeftWallCollision ->
                ( { model
                    | ballPosition = ( 0.1, ballY )
                    , ballVelocity = ( abs ballVelocityX, ballVelocityY )
                    , currentCollision = LeftWallCollision
                  }
                , Cmd.none
                )

            LeftPaddleCollision ->
                ( { model
                    | ballPosition = ( ballX, 87.9 )
                    , ballVelocity = ( -1 * abs ballVelocityX, -1 * abs ballVelocityY )
                    , currentCollision = LeftPaddleCollision
                  }
                , Cmd.none
                )

            LeftMiddlePaddleCollision ->
                ( { model
                    | ballPosition = ( ballX, 87.9 )
                    , ballVelocity = ( ballVelocityX, -1 * abs ballVelocityY )
                    , currentCollision = LeftMiddlePaddleCollision
                  }
                , Cmd.none
                )

            MiddlePaddleCollision ->
                ( { model
                    | ballPosition = ( ballX, 87.9 )
                    , ballVelocity = ( ballVelocityX, -1 * abs ballVelocityY )
                    , currentCollision = MiddlePaddleCollision
                  }
                , Cmd.none
                )

            RightMiddlePaddleCollision ->
                ( { model
                    | ballPosition = ( ballX, 87.9 )
                    , ballVelocity = ( ballVelocityX, -1 * abs ballVelocityY )
                    , currentCollision = RightMiddlePaddleCollision
                  }
                , Cmd.none
                )

            RightPaddleCollision ->
                ( { model
                    | ballPosition = ( ballX, 87.9 )
                    , ballVelocity = ( abs ballVelocityX, -1 * abs ballVelocityY )
                    , currentCollision = RightPaddleCollision
                  }
                , Cmd.none
                )

            TopBlockCollision block ->
                let
                    ( updatedBallVelocityX, updatedBallVelocityY, updatedGameSpeed, updatedPaddle ) =
                        if block.yPosition < 16 && model.gameSpeed == Slow then
                            ( ballVelocityX * 2, ballVelocityY * 2, Fast, Paddle.shortPaddle )
                        else
                            ( ballVelocityX, ballVelocityY, model.gameSpeed, model.paddle )
                in
                    ( { model
                        | ballPosition = ( ballX, (toFloat block.yPosition) - 2 )
                        , ballVelocity = ( updatedBallVelocityX, -1 * abs updatedBallVelocityY )
                        , blocks = List.filter (\b -> b /= block) model.blocks
                        , currentCollision = TopBlockCollision block
                        , gameSpeed = updatedGameSpeed
                        , paddle = updatedPaddle
                      }
                    , Cmd.none
                    )

            BottomBlockCollision block ->
                let
                    ( updatedBallVelocityX, updatedBallVelocityY, updatedGameSpeed, updatedPaddle ) =
                        if block.yPosition < 16 && model.gameSpeed == Slow then
                            ( ballVelocityX * 2, ballVelocityY * 2, Fast, Paddle.shortPaddle )
                        else
                            ( ballVelocityX, ballVelocityY, model.gameSpeed, model.paddle )
                in
                    ( { model
                        | ballPosition = ( ballX, (toFloat block.yPosition) + 2 )
                        , ballVelocity = ( updatedBallVelocityX, abs updatedBallVelocityY )
                        , blocks = List.filter (\b -> b /= block) model.blocks
                        , currentCollision = BottomBlockCollision block
                        , gameSpeed = updatedGameSpeed
                        , paddle = updatedPaddle
                      }
                    , Cmd.none
                    )


collectCollisions : Time -> Model -> Maybe Collision
collectCollisions dt model =
    detectCollisions dt model
        |> List.filterMap identity
        |> List.head


detectCollisions : Time -> Model -> List (Maybe Collision)
detectCollisions dt model =
    let
        wallCollision =
            detectWallCollision model.ballPosition

        paddleCollision =
            detectPaddleCollision model.ballPosition model.paddlePosition model.paddle

        blockCollision =
            detectBlockCollision model.currentCollision model.ballPosition model.ballVelocity model.blocks
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
    if ballPosY > 88 && ballPosY < 89 then
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
            if ballPosY < 22 || ballPosY >= 10 then
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
            paddlePosition - 1
        else
            paddlePosition
    else if gameControls.paddleRight then
        case paddle of
            Paddle.NormalPaddle paddleSize ->
                if paddlePosition < 80 then
                    paddlePosition + 1
                else
                    paddlePosition

            Paddle.ShortPaddle paddleSize ->
                if paddlePosition < 90 then
                    paddlePosition + 1
                else
                    paddlePosition
    else
        paddlePosition



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
            ++ [ Paddle.renderPaddle model.paddlePosition model.paddle ]
        )



---- PROGRAM ----


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.gameState of
        Playing ->
            Sub.batch
                [ AnimationFrame.diffs TimeUpdate
                , Keyboard.Extra.downs <| keyToGameControl True
                , Keyboard.Extra.ups <| keyToGameControl False
                ]

        _ ->
            Keyboard.Extra.downs KeyboardPress


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
