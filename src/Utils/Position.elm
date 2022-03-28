module Utils.Position exposing (..)

import Model exposing (Position, Tile(..), TileSpace)


getFreeTilePositions : TileSpace -> List Position
getFreeTilePositions =
    let
        getFreePositionsFromRow =
            List.filterMap
                (\tile ->
                    case tile of
                        Free pos ->
                            Just pos

                        _ ->
                            Nothing
                )
    in
    List.foldl (\row acc -> getFreePositionsFromRow row |> List.append acc) []
