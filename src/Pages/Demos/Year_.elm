module Pages.Demos.Year_ exposing (Model, Msg, page)

import Auth
import Effect exposing (Effect)
import Layouts
import Page exposing (Page)
import Route exposing (Route)
import Rss exposing (Post)
import Shared
import Time
import View exposing (View)
import View.Demo


page : Auth.User -> Shared.Model -> Route { year : String } -> Page Model Msg
page { rss } _ route =
    Page.new
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view route.params rss
        }
        |> Page.withLayout (\_ -> Layouts.Default {})



-- INIT


type alias Model =
    {}


init : () -> ( Model, Effect Msg )
init () =
    ( {}
    , Effect.none
    )



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        NoOp ->
            ( model
            , Effect.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : { year : String } -> { a | posts : List Post } -> Model -> View Msg
view { year } { posts } _ =
    { title = year ++ "'s demos"
    , body =
        List.filterMap
            (\post ->
                case post.title of
                    Rss.Demo number title ->
                        let
                            yearString =
                                post.pubDate
                                    |> Time.toYear
                                        -- (Yes, I know UTC is wrong half of the year, shush, we're getting the Year and Orla doesn't post at midnight on new Year's eve)
                                        Time.utc
                                    |> String.fromInt
                        in
                        if yearString == year then
                            Just (View.Demo.view number title post)

                        else
                            Nothing

                    _ ->
                        Nothing
            )
            posts
    }
