module Utils.Input exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Model exposing (BlockShape(..), Color(..), MouseMoveData, Msg(..))


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
