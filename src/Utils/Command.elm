module Utils.Command exposing (..)

import Array
import Model exposing (BlockShape(..), Color(..), Msg(..))
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
