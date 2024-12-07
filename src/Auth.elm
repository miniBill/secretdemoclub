module Auth exposing (User, onPageLoad, viewCustomPage)

import Auth.Action
import Dict
import Route exposing (Route)
import Route.Path
import Rss exposing (Rss)
import Shared
import View exposing (View)


type alias User =
    { rss : { url : String, rss : Rss } }


{-| Called before an auth-only page is loaded.
-}
onPageLoad : Shared.Model -> Route () -> Auth.Action.Action User
onPageLoad shared route =
    case shared.rss of
        Just data ->
            Auth.Action.loadPageWithUser { rss = data }

        Nothing ->
            Auth.Action.pushRoute
                { path = Route.Path.LoadRSS
                , query = Dict.fromList [ ( "from", route.url.path ) ]
                , hash = Nothing
                }


{-| Renders whenever `Auth.Action.loadCustomPage` is returned from `onPageLoad`.
-}
viewCustomPage : Shared.Model -> Route () -> View Never
viewCustomPage _ _ =
    View.none
