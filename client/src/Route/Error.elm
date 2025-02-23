module Route.Error exposing (view)

import ConcurrentTask.Http as Http
import Html
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
