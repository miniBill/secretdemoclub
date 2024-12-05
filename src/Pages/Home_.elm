module Pages.Home_ exposing (Model, Msg, page)

import Dict
import Html
import Layouts
import Page exposing (Page)
import Route
import Route.Path as Path
import View exposing (View)


type alias Model =
    {}


type alias Msg =
    {}


page : Page Model Msg
page =
    Page.sandbox { init = {}, view = view, update = always }
        |> Page.withLayout (\_ -> Layouts.Default {})


view : model -> View msg
view _ =
    { title = ""
    , body =
        [ Html.a
            [ Route.href
                { path = Path.Demos
                , query = Dict.empty
                , hash = Nothing
                }
            ]
            [ Html.text "Demos" ]
        , List.range 2015 2025
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
        ]
    }
