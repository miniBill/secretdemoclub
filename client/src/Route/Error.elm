module Route.Error exposing (view)

import Html exposing (Html)
import Html.Attributes
import Http
import View exposing (View)


view : Http.Error -> View msg
view error =
    { content =
        ( []
        , [ Html.p [] (errorToParagraph error) ]
        )
    , title = Just "=("
    }


errorToParagraph : Http.Error -> List (Html msg)
errorToParagraph error =
    case error of
        Http.BadBody body ->
            [ Html.text "Unexpected answer from the server - content"
            , Html.pre [ Html.Attributes.style "display" "none" ] [ Html.text body ]
            ]

        Http.BadUrl _ ->
            [ Html.text "Internal error - url" ]

        Http.Timeout ->
            [ Html.text "Timeout" ]

        Http.NetworkError ->
            [ Html.text "Network error" ]

        Http.BadStatus _ ->
            [ Html.text "Unexpected answer from the server - status" ]
