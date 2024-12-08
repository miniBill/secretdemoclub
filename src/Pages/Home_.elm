module Pages.Home_ exposing (Model, Msg, page)

import Dict
import Effect
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Layouts
import List.Extra
import Maybe.Extra
import Page exposing (Page)
import Route exposing (Route)
import Route.Path as Path
import Shared
import Time
import View exposing (View)
import View.Post


type alias Model =
    { search : String }


type Msg
    = Search String


page : Shared.Model -> Route () -> Page Model Msg
page shared _ =
    Page.new
        { init = init
        , view = view shared
        , update = update
        , subscriptions = \_ -> Sub.none
        }
        |> Page.withLayout (\_ -> Layouts.Default {})


init : () -> ( Model, Effect.Effect Msg )
init _ =
    ( { search = "" }, Effect.none )


update : Msg -> Model -> ( Model, Effect.Effect Msg )
update msg model =
    case msg of
        Search search ->
            ( { model | search = search }, Effect.none )


view : Shared.Model -> Model -> View Msg
view shared model =
    let
        toolbar : List (Html Msg)
        toolbar =
            [ Html.label []
                [ Html.text "Search "
                , Html.input
                    [ Html.Attributes.type_ "search"
                    , Html.Attributes.value model.search
                    , Html.Events.onInput Search
                    ]
                    []
                ]
            ]
    in
    if String.isEmpty model.search || List.isEmpty shared.rss.posts then
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
        , toolbar = toolbar
        }

    else
        { title = ""
        , body =
            shared.rss.posts
                |> List.filterMap
                    (\post ->
                        if View.Post.isMatch model.search post then
                            Just (View.Post.view shared { showKind = True } post)

                        else
                            Nothing
                    )
                |> Html.div
                    [ Html.Attributes.style "display" "flex"
                    , Html.Attributes.style "flex-wrap" "wrap"
                    , Html.Attributes.style "gap" "8px"
                    , Html.Attributes.style "align-items" "stretch"
                    ]
                |> List.singleton
        , toolbar = toolbar
        }
