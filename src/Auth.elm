module Auth exposing (User, onPageLoad, viewCustomPage)

import Auth.Action
import Dict
import Route exposing (Route)
import Route.Path
import Rss exposing (Post)
import Shared
import View exposing (View)


type alias User =
    { rss : { url : String, posts : List Post } }


{-| Called before an auth-only page is loaded.
-}
onPageLoad : Shared.Model -> Route () -> Auth.Action.Action User
onPageLoad shared route =
    if List.isEmpty shared.rss.posts then
        Auth.Action.pushRoute
            { path = Route.Path.LoadRss
            , query = Dict.fromList [ ( "from", route.url.path ) ]
            , hash = Nothing
            }

    else
        Auth.Action.loadPageWithUser { rss = shared.rss }


{-| Renders whenever `Auth.Action.loadCustomPage` is returned from `onPageLoad`.
-}
viewCustomPage : Shared.Model -> Route () -> View Never
viewCustomPage _ _ =
    View.none
