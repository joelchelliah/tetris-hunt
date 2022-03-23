module Main exposing (..)

import Browser
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown, onMouseMove)
import Components.Hud as Hud
import Components.Tiles as Tiles
import Html exposing (div, h1, text)
import Html.Attributes exposing (class)
import Json.Decode as Decode exposing (Decoder)
import Model exposing (GameState(..), Model, MouseMoveData, Msg(..), config)
import Utils.Icon exposing (iconCss)


init : () -> ( Model, Cmd Msg )
init () =
    ( { state = Init
      , blocks = []
      , tileSpace = Tiles.init config.gameWidth config.gameHeight
      , frameDeltas = { tickDelta = 0 }
      , mouseMoveDebug = { offsetX = 0, offsetY = 0, width = 0, height = 0 }
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Enter ->
            let
                newState =
                    if model.state == Running then
                        Paused

                    else
                        Running
            in
            ( { model | state = newState }, Cmd.none )

        MouseMove data ->
            ( { model | mouseMoveDebug = data }, Cmd.none )

        _ ->
            ( model, Cmd.none )


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


keyToMessage : String -> Msg
keyToMessage key =
    case key of
        "Enter" ->
            Enter

        _ ->
            Tick


keyboardDecoder : Decoder Msg
keyboardDecoder =
    let
        toMsg =
            \val ->
                if val == "Enter" then
                    Enter

                else
                    Noop
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
                , mouseMoveSub
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
