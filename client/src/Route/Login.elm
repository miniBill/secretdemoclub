module Route.Login exposing (Model, Msg, page)

import Auth
import Effect exposing (Effect)
import Html
import Html.Attributes
import Html.Events
import Layouts
import Page exposing (Page)
import Route exposing (Route)
import Shared
import View exposing (View)
import View.Post


page : Shared.Model -> Route () -> Page Model Msg
page shared _ =
    Page.new
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view shared
        }
        |> Page.withLayout (\_ -> Layouts.Default {})
