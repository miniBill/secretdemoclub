module Pages.Demos.Year_ exposing (Model, Msg, page)

import Auth
import Effect exposing (Effect)
import Html
import Html.Attributes
import Html.Events
import Layouts
import Page exposing (Page)
import Route exposing (Route)
import Rss exposing (Post)
import Shared
import Time
import View exposing (View)
import View.Post


page : Auth.User -> Shared.Model -> Route { year : String } -> Page Model Msg
page _ shared route =
    Page.new
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view route.params shared
        }
        |> Page.withLayout (\_ -> Layouts.Default {})



-- INIT


type alias Model =
    { search : String }


init : () -> ( Model, Effect Msg )
init () =
    ( { search = "" }
    , Effect.none
    )



-- UPDATE


type Msg
    = Search String


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        Search search ->
            ( { model | search = search }, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : { year : String } -> Shared.Model -> Model -> View Msg
view { year } shared model =
    { title = year ++ "'s demos"
    , body =
        shared.rss.posts
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
                                Just (View.Post.view shared { showKind = False } post)

                            else
                                Nothing

                        _ ->
                            Nothing
                )
            |> Html.div
                [ Html.Attributes.style "display" "flex"
                , Html.Attributes.style "flex-wrap" "wrap"
                , Html.Attributes.style "gap" "8px"
                , Html.Attributes.style "align-items" "stretch"
                ]
            |> List.singleton
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
