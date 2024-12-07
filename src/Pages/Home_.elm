module Pages.Home_ exposing (Model, Msg, page)

import Dict
import Effect
import Html
import Layouts
import List.Extra
import Maybe.Extra
import Page exposing (Page)
import Route exposing (Route)
import Route.Path as Path
import Shared
import Time
import View exposing (View)


type alias Model =
    {}


type alias Msg =
    {}


page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = \_ -> ( {}, Effect.none )
        , view = view shared
        , update = \_ model -> ( model, Effect.none )
        , subscriptions = \_ -> Sub.none
        }
        |> Page.withLayout (\_ -> Layouts.Default {})


view : Shared.Model -> model -> View msg
view shared _ =
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
        , let
            lastPosted =
                shared.rss.posts
                    |> List.Extra.last
                    |> Maybe.map .pubDate
                    |> Maybe.Extra.orElseLazy (\_ -> Maybe.map Tuple.second shared.time)

            here =
                shared.time
                    |> Maybe.map Tuple.first
                    |> Maybe.withDefault Time.utc

            maybeLastYear =
                Maybe.map (Time.toYear here) lastPosted
          in
          case maybeLastYear of
            Nothing ->
                Html.text ""

            Just lastYear ->
                List.range 2015 lastYear
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
