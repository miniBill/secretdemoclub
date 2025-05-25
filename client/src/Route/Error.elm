module Route.Error exposing (view)

import Html
import Http
import View exposing (View)


view : Http.Error -> View msg
view error =
    { body =
        [ Html.div []
            [ Html.p []
                [ Html.text (errorToString error)
                ]
            ]
        ]
    , title = Just "=("
    }


errorToString : Http.Error -> String
errorToString error =
    case error of
        Http.BadBody _ ->
            "Unexpected answer from the server - content"

        Http.BadUrl _ ->
            "Internal error - url"

        Http.Timeout ->
            "Timeout"

        Http.NetworkError ->
            "Network error"

        Http.BadStatus _ ->
            "Unexpected answer from the server - status"
