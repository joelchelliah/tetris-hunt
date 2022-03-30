module Message exposing (..)

import Array
import Json.Decode as Decode exposing (Decoder)
import Model exposing (BlockShape(..), Color(..), MouseMoveData, Msg(..), Position)
import Random


getNewBlockCommand : Cmd Msg
getNewBlockCommand =
    let
        shapes =
            Array.fromList [ Square, Line, Pyramid, LeftFoot, RightFoot, LeftSnake, RightSnake ]

        toShape i =
            case Array.get i shapes of
                Just shape ->
                    shape

                Nothing ->
                    Square
    in
    (Array.length shapes - 1)
        |> Random.int 0
        |> Random.map toShape
        |> Random.generate NewBlock


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


mouseClickDecoder : Decoder Msg
mouseClickDecoder =
    Decode.succeed MouseClick
