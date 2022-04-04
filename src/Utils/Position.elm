module Utils.Position exposing (..)

import Model exposing (Position, Tile(..), TileSpace)


getFreeTilePositions : TileSpace -> List Position
getFreeTilePositions =
    let
        getFromRow =
            List.filterMap
                (\tile ->
                    case tile of
                        Free pos ->
                            Just pos

                        _ ->
                            Nothing
                )
    in
    List.foldl (\row acc -> getFromRow row |> List.append acc) []


getTopMostDecayedTilePosition : TileSpace -> Maybe Position
getTopMostDecayedTilePosition tileSpace =
    let
        firstTileFromEachRow =
            List.foldl (\row acc -> List.head row :: acc) [] tileSpace

        getTopMostDecayedPos nextTile foundPos =
            case ( nextTile, foundPos ) of
                ( _, Just _ ) ->
                    foundPos

                ( Just (Decayed pos), _ ) ->
                    Just pos

                _ ->
                    foundPos
    in
    List.foldl getTopMostDecayedPos Nothing firstTileFromEachRow


isHigher : Position -> Position -> Bool
isHigher ( _, y1 ) ( _, y2 ) =
    y1 < y2


areValidPositions : TileSpace -> List Position -> Bool
areValidPositions tileSpace =
    let
        isPositionOutOfView ( _, y ) =
            y < 0

        isOnFreeTile pos =
            isPositionOutOfView pos
                || (getFreeTilePositions tileSpace
                        |> List.member pos
                   )
    in
    List.all isOnFreeTile


diffPositions : List Position -> List Position -> List Position
diffPositions =
    let
        sub ( x1, y1 ) ( x2, y2 ) =
            ( x1 - x2, y1 - y2 )
    in
    List.map2 sub


offsetPositions : List Position -> List Position -> List Position
offsetPositions =
    let
        add ( oX, oY ) ( pX, pY ) =
            ( oX + pX, oY + pY )
    in
    List.map2 add
