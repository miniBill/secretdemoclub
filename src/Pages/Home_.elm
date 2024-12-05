module Pages.Home_ exposing (page)

import Dict
import Html
import Route
import Route.Path as Path
import View exposing (View)


page : View msg
page =
    { title = "Homepage"
    , body =
        [ 2024, 2025 ]
            |> List.map
                (\year ->
                    Html.li []
                        [ Html.a
                            [ Route.href
                                { path = Path.Demos_Year_ { year = String.fromInt year }
                                , query = Dict.empty
                                , hash = Nothing
                                }
                            ]
                            [ Html.text (String.fromInt year) ]
                        ]
                )
            |> Html.ul []
            |> List.singleton
    }
