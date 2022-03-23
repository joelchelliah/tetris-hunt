module Components.Tiles exposing (..)

import Html exposing (Attribute, Html, div, span)
import Html.Attributes exposing (class)
import Model exposing (Model, Msg, Tile(..), TileSpace)


type alias Container msg =
    List (Attribute msg) -> List (Html msg) -> Html msg


type TileClass
    = FreeTile
    | Tile String


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


viewTile : Tile -> Html Msg
viewTile tile =
    case tile of
        Wall ->
            placeWallTile

        Free pos ->
            placeFreeTile

        _ ->
            div [] []


init : Int -> Int -> TileSpace
init width height =
    let
        initTile ( x, y ) =
            if x == 1 || x == width || y == height then
                Wall

            else
                Free ( x, y )

        initRow y =
            List.map (\x -> initTile ( x, y )) (List.range 1 width)
    in
    List.map initRow (List.range 1 height)


view : Model -> Html Msg
view { tileSpace } =
    let
        viewRow row =
            div [ class "row" ] (List.map viewTile row)
    in
    div [] (List.map viewRow tileSpace)
