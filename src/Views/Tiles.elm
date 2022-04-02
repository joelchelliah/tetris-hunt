module Views.Tiles exposing (..)

import Components.Block as Block
import Html exposing (Attribute, Html, div, span)
import Html.Attributes exposing (class)
import Model exposing (Block, BlockState(..), Color(..), Model, Msg, Tile(..))


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


makeTile : Container msg -> Bool -> TileClass -> Html msg
makeTile tileContainer isPreview tileClass =
    let
        wrapInBackgroundTile tile =
            if isPreview then
                div [ class "preview-tile" ] [ tile ]

            else
                div [ class "background-tile" ] [ tile ]
    in
    case tileClass of
        FreeTile ->
            tileContainer [] [] |> wrapInBackgroundTile

        Tile name ->
            tileContainer [ class ("tile " ++ name) ] [] |> wrapInBackgroundTile


placeWallTile : Html msg
placeWallTile =
    Tile "wall" |> makeTile div False


placeDecayingTile : Html msg
placeDecayingTile =
    Tile "decaying" |> makeTile div False


placeFreeTile : Html msg
placeFreeTile =
    makeTile span False FreeTile


placePreviewTile : Html msg
placePreviewTile =
    makeTile span True FreeTile


placeBlockTile : Color -> Html msg
placeBlockTile color =
    Tile ("block " ++ colorToString color) |> makeTile div False


placePreviewBlockTile : Color -> Html msg
placePreviewBlockTile color =
    Tile ("block " ++ colorToString color ++ " preview") |> makeTile div True


viewTile : Maybe Block -> Tile -> Html Msg
viewTile block tile =
    case tile of
        Wall ->
            placeWallTile

        Locked { color } ->
            placeBlockTile color

        Falling { color } _ ->
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

        Decaying _ _ ->
            -- TODO: Removing animation based on float param
            placeDecayingTile


previewTile : Maybe Block -> Tile -> Html Msg
previewTile block tile =
    case tile of
        Free pos ->
            case block of
                Nothing ->
                    placePreviewTile

                Just { positions, color } ->
                    if List.member pos positions then
                        placePreviewBlockTile color

                    else
                        placePreviewTile

        _ ->
            div [] []


preview : Model -> Html Msg
preview { previewSpace, block } =
    let
        previewRow row =
            div [ class "row preview" ] (List.map (previewTile block) row)
    in
    div []
        [ div [ class "preview-backdrop" ] []
        , div [ class "preview-space" ] (List.map previewRow previewSpace)
        ]


view : Model -> Html Msg
view ({ tileSpace, block } as model) =
    let
        viewRow movingBlock row =
            div [ class "row" ] (List.map (viewTile movingBlock) row)
    in
    if Block.hasState Spinning block then
        div []
            [ preview model
            , div [] (List.map (viewRow Nothing) tileSpace)
            ]

    else
        div [] (List.map (viewRow block) tileSpace)
