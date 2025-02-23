module Route.Loading exposing (view)

import Html
import View exposing (View)


view : View msg
view =
    { body =
        [ Html.div []
            [ Html.p []
                [ Html.text "Loading..."
                ]
            ]
        ]
    , title = "=("
    }
