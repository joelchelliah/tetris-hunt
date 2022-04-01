module Main exposing (..)

import Browser
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown, onMouseDown, onMouseMove)
import Components.Block as Block
import Components.TileSpace as TileSpace
import Html exposing (Html, div, h1, text)
import Html.Attributes exposing (class)
import Model exposing (BlockShape(..), BlockState(..), GameState(..), Model, Msg(..), config)
import Utils.Command exposing (getNewBlockCommand)
import Utils.Icon exposing (iconCss)
import Utils.Input exposing (keyboardDecoder, mouseClickDecoder, mouseMoveDecoder)
import Views.Hud as Hud
import Views.Tiles as Tiles


init : () -> ( Model, Cmd Msg )
init () =
    ( { state = Init
      , block = Nothing
      , tileSpace = TileSpace.init config.gameWidth config.gameHeight
      , previewSpace = TileSpace.init 7 5
      , frameDeltas = { moveTickDelta = 0, spinTickDelta = 0 }
      , mouseMoveDebug = { offsetX = 0, offsetY = 0, width = 0, height = 0 }
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ state, block, tileSpace, frameDeltas } as model) =
    case msg of
        Enter ->
            if state == Running then
                ( { model | state = Paused }, Cmd.none )

            else if state == GameOver then
                init ()

            else
                ( { model | state = Running }, Cmd.none )

        MouseMove data ->
            ( { model | mouseMoveDebug = data }, Cmd.none )

        MouseClick ->
            if Block.isSpinning block then
                ( { model | block = Block.stopSpin block }, Cmd.none )

            else
                ( model, Cmd.none )

        NewBlock shape ->
            ( { model | block = Just (Block.init shape) }, Cmd.none )

        SpinTick ->
            case block of
                Nothing ->
                    ( model, getNewBlockCommand )

                Just _ ->
                    ( { model | block = Block.updateSpin block }, Cmd.none )

        MoveTick ->
            if Block.isMoving block then
                let
                    newBlock =
                        Block.updateMovement tileSpace block
                in
                if block /= newBlock then
                    ( { model
                        | tileSpace = TileSpace.update Nothing tileSpace
                        , block = newBlock
                      }
                    , Cmd.none
                    )

                else if Block.isOutOfView newBlock then
                    ( { model | state = GameOver }, Cmd.none )

                else
                    ( { model
                        | tileSpace = TileSpace.update newBlock tileSpace
                        , block = Nothing
                      }
                    , getNewBlockCommand
                    )

            else
                ( model, Cmd.none )

        FrameDelta delta ->
            let
                newMoveTickDelta =
                    frameDeltas.moveTickDelta + delta

                newSpinTickDelta =
                    frameDeltas.spinTickDelta + delta

                updateDeltas move spin =
                    { frameDeltas | moveTickDelta = move, spinTickDelta = spin }
            in
            if newMoveTickDelta >= config.moveTickDelay then
                update MoveTick { model | frameDeltas = updateDeltas 0 newSpinTickDelta }

            else if newSpinTickDelta >= config.spinTickDelay then
                update SpinTick { model | frameDeltas = updateDeltas newMoveTickDelta 0 }

            else
                ( { model | frameDeltas = updateDeltas newMoveTickDelta newSpinTickDelta }, Cmd.none )


view : Model -> Html Msg
view model =
    let
        title =
            case model.state of
                Init ->
                    h1 [] [ text "🏁 Press Enter to start" ]

                Paused ->
                    h1 [] [ text "🕰️ Paused..." ]

                GameOver ->
                    h1 [] [ text "☠️ Game over!" ]

                Running ->
                    h1 [] [ text "🏃 Running!" ]

        mouseDebug =
            div [ class "debug-stuff" ]
                [ div [] [ text ("Pos X : " ++ String.fromInt model.mouseMoveDebug.offsetX) ]
                , div [] [ text ("Pos Y : " ++ String.fromInt model.mouseMoveDebug.offsetY) ]
                , div [] [ text ("Width : " ++ String.fromFloat model.mouseMoveDebug.width) ]
                , div [] [ text ("Height : " ++ String.fromFloat model.mouseMoveDebug.height) ]
                ]
    in
    div [ class "game" ]
        [ iconCss
        , title
        , mouseDebug
        , Tiles.view model
        , Hud.viewFooter
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        keyDownSub =
            onKeyDown keyboardDecoder

        mouseMoveSub =
            onMouseMove mouseMoveDecoder

        mouseClickSub =
            onMouseDown mouseClickDecoder
    in
    case model.state of
        Running ->
            Sub.batch
                [ onAnimationFrameDelta FrameDelta
                , mouseClickSub
                , keyDownSub

                -- TODO: Currently disabled to focus on basic gameplay elements first.
                -- , mouseMoveSub
                ]

        _ ->
            keyDownSub


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
