module Main exposing (..)

import Browser
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown, onMouseDown, onMouseMove)
import Components.Block as Block
import Components.TileSpace as TileSpace
import Html exposing (Html, div, h1, text)
import Html.Attributes exposing (class)
import Json.Decode as Decode exposing (Decoder)
import Message exposing (getNewBlockCommand, keyboardDecoder, mouseClickDecoder, mouseMoveDecoder)
import Model exposing (BlockShape(..), GameState(..), Model, MouseMoveData, Msg(..), config)
import Utils.Icon exposing (iconCss)
import Views.Hud as Hud
import Views.Tiles as Tiles


init : () -> ( Model, Cmd Msg )
init () =
    ( { state = Init
      , block = Nothing
      , tileSpace = TileSpace.init config.gameWidth config.gameHeight
      , frameDeltas = { tickDelta = 0 }
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
            ( { model | block = Block.rotate tileSpace block }, Cmd.none )

        NewBlock shape ->
            ( { model | block = Just (Block.init shape) }, Cmd.none )

        Tick ->
            let
                ( newBlock, didFall ) =
                    Block.fall tileSpace block
            in
            if didFall then
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

        FrameDelta delta ->
            let
                newTickDelta =
                    frameDeltas.tickDelta + delta

                updateDeltas tick =
                    { frameDeltas | tickDelta = tick }
            in
            if newTickDelta >= config.gameSpeed then
                update Tick { model | frameDeltas = updateDeltas 0 }

            else
                ( { model | frameDeltas = updateDeltas newTickDelta }, Cmd.none )

        _ ->
            ( model, Cmd.none )


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
