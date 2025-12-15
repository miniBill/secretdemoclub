module Route.Login exposing (loginUrl, view)

import Env
import Html exposing (Html)
import Html.Attributes as HA
import Theme
import Types exposing (Theme)
import Url exposing (Url)
import Url.Builder
import View exposing (View)


view :
    { model
        | root : Url
        , theme : Theme
    }
    -> View msg
view model =
    { title = Just "Login required"
    , content =
        Html.div
            [ HA.style "display" "flex"
            , HA.style "align-items" "center"
            , HA.style "justify-content" "center"
            , HA.style "flex" "1 0"
            ]
            [ loginButton model ]
    }


loginButton :
    { model
        | root : Url
        , theme : Theme
    }
    -> Html msg
loginButton model =
    Theme.linkButton [ HA.style "padding" "16px 32px" ]
        { theme = model.theme
        , href = loginUrl model
        , text = "Login"
        }


loginUrl : { model | root : Url } -> String
loginUrl model =
    Url.Builder.crossOrigin "https://www.patreon.com"
        [ "oauth2", "authorize" ]
        [ Url.Builder.string "response_type" "code"
        , Url.Builder.string "client_id" Env.clientId
        , Url.Builder.string "redirect_uri" (Url.toString model.root)
        , Url.Builder.string "scope" "identity identity.memberships"
        ]
