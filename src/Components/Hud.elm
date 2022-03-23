module Components.Hud exposing (..)

import Html exposing (Html, a, div, text)
import Html.Attributes exposing (class, href)
import Model exposing (GameState(..), Msg)
import Utils.Icon exposing (viewGithubIcon)


viewFooter : Html Msg
viewFooter =
    div
        [ class "github" ]
        [ viewGithubIcon
        , a [ href "https://github.com/joelchelliah/tetris-hunt" ]
            [ text "Find me on Github" ]
        ]
