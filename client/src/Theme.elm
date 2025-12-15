module Theme exposing (column, gap, linkButton, padding, row)

import Html exposing (Attribute, Html)
import Html.Attributes as HA
import Types exposing (Theme(..))


linkButton :
    List (Attribute msg)
    ->
        { theme : Theme
        , href : String
        , text : String
        }
    -> Html msg
linkButton attrs config =
    Html.a
        ([ HA.href config.href
         , HA.style "display" "inline-block"
         , padding
         , case config.theme of
            Dark ->
                HA.style "background" "var(--red)"

            Light ->
                HA.style "background" "var(--navy)"
         , HA.style "color" "var(--offwhite)"
         , HA.style "border-radius" "999px"
         ]
            ++ attrs
        )
        [ Html.text config.text ]


column : List (Attribute msg) -> List (Html msg) -> Html msg
column attrs children =
    Html.div
        (HA.style "display" "flex"
            :: HA.style "flex-direction" "column"
            :: gap
            :: attrs
        )
        children


row : List (Attribute msg) -> List (Html msg) -> Html msg
row attrs children =
    Html.div
        (HA.style "display" "flex"
            :: gap
            :: attrs
        )
        children


gap : Attribute msg
gap =
    HA.style "gap" "8px"


padding : Html.Attribute msg
padding =
    HA.style "padding" "8px"
