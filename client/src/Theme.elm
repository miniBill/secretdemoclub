module Theme exposing (linkButton)

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
         , HA.style "padding" "8px"
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
