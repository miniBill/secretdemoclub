module Route.Error exposing (view)

import Html
import Http
import View exposing (View)


view : Http.Error -> View msg
view error =
    { body =
        [ Html.div []
            [ Html.p []
                [ Html.text (Debug.toString error)
                ]
            ]
        ]
    , title = "=("
    }
