module Route.Demos.Year_ exposing (Data, Model, Msg, RouteParams, route)

import Effect exposing (Effect)
import Html
import Html.Attributes
import Html.Events
import Route exposing (Route)
import RouteBuilder exposing (StatelessRoute)
import Shared
import Time
import View exposing (View)
import View.Post


type alias Model =
    { search : String }


type Msg
    = Search String
    | Play String


type alias RouteParams =
    { year : String }


route : StatelessRoute RouteParams Data ActionData
route =
    RouteBuilder.preRenderWithFallback
        { head = head
        , pages = pages
        , data = data
        }
        |> RouteBuilder.buildWithLocalState { view = view }


page : Auth.User -> Shared.Model -> Route { year : String } -> Page Model Msg
page _ shared route =
    Page.new
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view route.params shared
        }
        |> Page.withLayout (\_ -> Layouts.Default {})


type alias Data =
    { something : String
    }


type alias ActionData =
    {}


init : () -> ( Model, Effect Msg )
init () =
    ( { search = "" }
    , Effect.none
    )


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        Search search ->
            ( { model | search = search }, Effect.none )

        Play url ->
            ( model, Effect.play url )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : { year : String } -> Shared.Model -> Model -> View Msg
view { year } shared model =
    { title = year ++ "'s demos"
    , body =
        [ shared.rss.posts
            |> List.filterMap
                (\post ->
                    case post.title of
                        Rss.Demo _ _ ->
                            let
                                yearString =
                                    post.pubDate
                                        |> Time.toYear
                                            -- (Yes, I know UTC is wrong half of the year, shush, we're getting the Year and Orla doesn't post at midnight on new Year's eve)
                                            Time.utc
                                        |> String.fromInt
                            in
                            if yearString == year && View.Post.isMatch model.search post then
                                Just post

                            else
                                Nothing

                        _ ->
                            Nothing
                )
            |> View.Post.viewList Play shared
        ]
    , toolbar =
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
    }
