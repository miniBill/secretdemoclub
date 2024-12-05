module Pages.Demos exposing (Model, Msg, page)

import Auth
import Effect exposing (Effect)
import Html
import Html.Attributes
import Html.Events
import Layouts
import Page exposing (Page)
import Route exposing (Route)
import Rss
import Shared
import View exposing (View)
import View.Post


page : Auth.User -> Shared.Model -> Route () -> Page Model Msg
page _ shared _ =
    Page.new
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view shared
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
    | Play String


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        Search search ->
            ( { model | search = search }, Effect.none )

        Play url ->
            ( model, Effect.play url )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared model =
    { title = "demos"
    , body =
        [ shared.rss.posts
            |> List.filterMap
                (\post ->
                    case post.title of
                        Rss.Demo _ _ ->
                            if View.Post.isMatch model.search post then
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
