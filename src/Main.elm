module Main exposing (..)

import Browser
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown)
import Html exposing (div, h1, text)
import Json.Decode as Decode


type alias Model =
    { state : GameState }


type GameState
    = Init
    | Running


type Msg
    = FrameDelta Float
    | Enter
    | Tick


init : () -> ( Model, Cmd Msg )
init () =
    ( { state = Init }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


view model =
    h1 [] [ text "LETS DO THIS!" ]


keyToMessage : String -> Msg
keyToMessage key =
    case key of
        "Enter" ->
            Enter

        _ ->
            Tick


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        keyDownSubscription =
            Decode.field "key" Decode.string |> Decode.map keyToMessage |> onKeyDown
    in
    case model.state of
        Running ->
            Sub.batch
                [ onAnimationFrameDelta FrameDelta
                ]

        _ ->
            keyDownSubscription


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
