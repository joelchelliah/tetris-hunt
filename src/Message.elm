module Message exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Model exposing (BlockShape(..), Color(..), MouseMoveData, Msg(..), Position)
import Random


getNewBlockCommand : Cmd Msg
getNewBlockCommand =
    let
        toShape i =
            case i of
                1 ->
                    Square

                2 ->
                    Line

                3 ->
                    Hook

                4 ->
                    ReverseHook

                5 ->
                    Snake

                6 ->
                    ReverseSnake

                _ ->
                    Pyramid
    in
    Random.int 1 7 |> Random.map toShape |> Random.generate NewBlock


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
