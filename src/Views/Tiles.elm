module Views.Tiles exposing (..)

import Html exposing (Attribute, Html, div, span)
import Html.Attributes exposing (class)
import Model exposing (Block, Color(..), Model, Msg, Tile(..))


type alias Container msg =
    List (Attribute msg) -> List (Html msg) -> Html msg


type TileClass
    = FreeTile
    | Tile String


colorToString : Color -> String
colorToString color =
    case color of
        Yellow ->
            "yellow"

        Teal ->
            "teal"

        Purple ->
            "purple"

        Orange ->
            "orange"

        Blue ->
            "blue"

        Green ->
            "green"

        Red ->
            "red"


makeTile : Container msg -> TileClass -> Html msg
makeTile tileContainer tileClass =
    let
        wrapInBackgroundTile tile =
            div [ class "background-tile" ] [ tile ]
    in
    case tileClass of
        FreeTile ->
            tileContainer [] [] |> wrapInBackgroundTile

        Tile name ->
            tileContainer [ class ("tile " ++ name) ] [] |> wrapInBackgroundTile


placeWallTile : Html msg
placeWallTile =
    Tile "wall" |> makeTile div


placeFreeTile : Html msg
placeFreeTile =
    makeTile span FreeTile


placeBlockTile : Color -> Html msg
placeBlockTile color =
    Tile ("block " ++ colorToString color) |> makeTile div


viewTile : Maybe Block -> Tile -> Html Msg
viewTile block tile =
    case tile of
        Wall ->
            placeWallTile

        Locked _ color ->
            placeBlockTile color

        UnLocked _ color ->
            placeBlockTile color

        Free pos ->
            case block of
                Nothing ->
                    placeFreeTile

                Just { positions, color } ->
                    if List.member pos positions then
                        placeBlockTile color

                    else
                        placeFreeTile

        Removing _ _ _ ->
            -- TODO: Removing animation based on float param
            placeWallTile


view : Model -> Html Msg
view { tileSpace, block } =
    let
        viewRow row =
            div [ class "row" ] (List.map (viewTile block) row)
    in
    div [] (List.map viewRow tileSpace)
