module Main exposing (..)

import Browser
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown, onMouseMove)
import Components.Blocks as Blocks
import Components.TileSpace as TileSpace
import Html exposing (Html, div, h1, text)
import Html.Attributes exposing (class)
import Json.Decode as Decode exposing (Decoder)
import Model exposing (GameState(..), Model, MouseMoveData, Msg(..), config)
import Utils.Icon exposing (iconCss)
import Views.Hud as Hud
import Views.Tiles as Tiles


init : () -> ( Model, Cmd Msg )
init () =
    ( { state = Init
      , block = Blocks.init
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
            let
                newState =
                    if state == Running then
                        Paused

                    else
                        Running
            in
            ( { model | state = newState }, Cmd.none )

        MouseMove data ->
            ( { model | mouseMoveDebug = data }, Cmd.none )

        Tick ->
            let
                ( newBlock, didDescend ) =
                    Blocks.update tileSpace block
            in
            if didDescend then
                ( { model
                    | tileSpace = TileSpace.update Nothing tileSpace
                    , block = newBlock
                  }
                , Cmd.none
                )

            else
                ( { model
                    | tileSpace = TileSpace.update newBlock tileSpace
                    , block = Nothing
                  }
                , --TODO: Message for generating next block
                  Cmd.none
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
                Running ->
                    h1 [] [ text "🏃 Running!" ]

                _ ->
                    h1 [] [ text "🕰️ Paused..." ]

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


keyboardDecoder : Decoder Msg
keyboardDecoder =
    let
        toMsg =
            \val ->
                if val == "Enter" then
                    Enter

                else
                    FrameDelta 0
    in
    Decode.field "key" Decode.string |> Decode.map toMsg


mouseMoveDecoder : Decoder Msg
mouseMoveDecoder =
    Decode.map4 MouseMoveData
        (Decode.field "clientX" Decode.int)
        (Decode.field "clientY" Decode.int)
        (Decode.at [ "target", "offsetWidth" ] Decode.float)
        (Decode.at [ "target", "offsetHeight" ] Decode.float)
        |> Decode.map MouseMove


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        keyDownSub =
            onKeyDown keyboardDecoder

        mouseMoveSub =
            onMouseMove mouseMoveDecoder
    in
    case model.state of
        Running ->
            Sub.batch
                [ onAnimationFrameDelta FrameDelta
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
