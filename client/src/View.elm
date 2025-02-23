module View exposing
    ( View, map
    , none, fromString
    )

{-|

@docs View, map
@docs none, fromString
@docs toBrowserDocument

-}

import Html exposing (Html)


type alias View msg =
    { title : String
    , body : List (Html msg)
    , toolbar : List (Html msg)
    }


{-| Used internally by Elm Land to connect your pages together.
-}
map : (msg1 -> msg2) -> View msg1 -> View msg2
map fn view =
    { title = view.title
    , body = List.map (Html.map fn) view.body
    , toolbar = List.map (Html.map fn) view.toolbar
    }


{-| Used internally by Elm Land whenever transitioning between
authenticated pages.
-}
none : View msg
none =
    { title = ""
    , body = []
    , toolbar = []
    }


{-| If you customize the `View` module, anytime you run `elm-land add page`,
the generated page will use this when adding your `view` function.

That way your app will compile after adding new pages, and you can see
the new page working in the web browser!

-}
fromString : String -> View msg
fromString moduleName =
    { title = moduleName
    , body = [ Html.text moduleName ]
    , toolbar = []
    }
