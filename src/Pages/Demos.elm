module Pages.Demos exposing (Model, Msg, page)

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
import View exposing (View)
import View.Post


page : Auth.User -> Shared.Model -> Route () -> Page Model Msg
page { rss } _ _ =
    Page.new
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view rss
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


view : { a | posts : List Post } -> Model -> View Msg
view { posts } model =
    { title = "demos"
    , body =
        posts
            |> List.filterMap
                (\post ->
                    case post.title of
                        Rss.Demo _ _ ->
                            if View.Post.isMatch model.search post then
                                Just (View.Post.view { showKind = False } post)

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
