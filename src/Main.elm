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
    , redBlocks : List Block
    , orangeBlocks : List Block
    , darkOrangeBlocks : List Block
    , yellowBlocks : List Block
    , greenBlocks : List Block
    , blueBlocks : List Block
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


type alias Block =
    { fillColor : String
    , value : Int
    , yPosition : Int
    , xPosition : Int
    }


initialVelocity : Float
initialVelocity =
    0.03


createBlockList : String -> Int -> Int -> List Block
createBlockList fillColor value yPosition =
    List.map (Block fillColor value yPosition) (List.map (multiplyByFive) (List.range 0 19))


multiplyByFive : Int -> Int
multiplyByFive num =
    num * 5


init : ( Model, Cmd Msg )
init =
    ( { gameState = NotPlaying
      , playerStats = Player 0 0 0
      , ballPosition = ( -5, -5 )
      , ballVelocity = ( initialVelocity, initialVelocity )
      , paddlePosition = 40
      , redBlocks = createBlockList "#CB4744" 10 10
      , orangeBlocks = createBlockList "#C76C3A" 8 12
      , darkOrangeBlocks = createBlockList "#B47830" 6 14
      , yellowBlocks = createBlockList "#9FA426" 4 16
      , greenBlocks = createBlockList "#46A047" 2 18
      , blueBlocks = createBlockList "#4546C9" 1 20
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

        allBlocks =
            model.redBlocks ++ model.orangeBlocks ++ model.darkOrangeBlocks ++ model.yellowBlocks ++ model.greenBlocks ++ model.blueBlocks

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
                    ( 60, initialVelocity, model.blueBlocks )

                88 ->
                    -- need the paddle position so the ball can change it's tragectory
                    if List.member roundedBallPositionX (List.range model.paddlePosition (model.paddlePosition + 20)) then
                        ( 87, -1 * abs ballVelocityY, model.blueBlocks )
                    else
                        ( ballPositionY, ballVelocityY, model.blueBlocks )

                21 ->
                    -- write function that detects collision with any of the blue blocks
                    ( 22, abs ballVelocityY, checkBlockCollision ballPositionX model.blueBlocks )

                0 ->
                    ( 0.5, abs ballVelocityY, model.blueBlocks )

                _ ->
                    ( ballPositionY, ballVelocityY, model.blueBlocks )
    in
        ( { model
            | ballPosition =
                ( newBallPositionX + newBallVelocityX * dt
                , newBallPositionY + newBallVelocityY * dt
                )
            , ballVelocity = ( newBallVelocityX, newBallVelocityY )
            , blueBlocks = newListOfBlocks
          }
        , Cmd.none
        )


checkBlockCollision : Float -> List Block -> List Block
checkBlockCollision ballX blocks =
    let
        debugthis =
            Debug.log "ballX" ballX
    in
        List.filter (\block -> not (List.member (round ballX) (List.range block.xPosition (block.xPosition + 5)))) blocks



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
        ((rowOfSvgBlocks model.redBlocks)
            ++ (rowOfSvgBlocks model.orangeBlocks)
            ++ (rowOfSvgBlocks model.darkOrangeBlocks)
            ++ (rowOfSvgBlocks model.yellowBlocks)
            ++ (rowOfSvgBlocks model.greenBlocks)
            ++ (rowOfSvgBlocks model.blueBlocks)
            ++ [ ball model.ballPosition ]
            ++ [ paddle model.paddlePosition ]
        )


rowOfSvgBlocks : List Block -> List (Html Msg)
rowOfSvgBlocks blocks =
    List.map svgBlock blocks


svgBlock : Block -> Html Msg
svgBlock { fillColor, value, yPosition, xPosition } =
    rect
        [ width "5%"
        , height "2"
        , fill fillColor
        , y (toString yPosition)
        , x ((toString xPosition) ++ "%")
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
