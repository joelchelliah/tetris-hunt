module Utils.Icon exposing (..)

import FontAwesome.Attributes as Icon
import FontAwesome.Brands as Icon
import FontAwesome.Icon as Icon exposing (Icon)
import FontAwesome.Layering as Icon
import FontAwesome.Solid as Icon
import FontAwesome.Styles as Icon
import FontAwesome.Transforms as Icon
import Html exposing (..)
import Html.Attributes exposing (..)


iconCss : Html msg
iconCss =
    Icon.css


viewGithubIcon : Html msg
viewGithubIcon =
    Icon.github |> Icon.present |> Icon.styled [ Icon.fa2x, Icon.pullLeft ] |> Icon.view
