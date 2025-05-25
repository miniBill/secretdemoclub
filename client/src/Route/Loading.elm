module Route.Loading exposing (view)

import Html
import View exposing (View)


view : View msg
view =
    { content =
        Html.p []
            [ Html.text "Loading..."
            ]
    , title = Just "Loading..."
    }
