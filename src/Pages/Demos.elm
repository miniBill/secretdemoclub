module Pages.Demos exposing (Model, Msg, page)

import Auth
import Effect exposing (Effect)
import Layouts
import Page exposing (Page)
import Route exposing (Route)
import Rss exposing (Rss)
import Shared
import View exposing (View)
import View.Demo


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
    {}


init : () -> ( Model, Effect Msg )
init () =
    ( {}, Effect.none )



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : { a | rss : Rss } -> Model -> View Msg
view { rss } _ =
    { title = "demos"
    , body =
        List.filterMap
            (\post ->
                case post.title of
                    Rss.Demo number title ->
                        Just (View.Demo.view number title post)

                    _ ->
                        Nothing
            )
            rss
    }
