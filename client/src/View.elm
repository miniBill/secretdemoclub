module View exposing (View)

{-|

@docs View

-}

import Html exposing (Html)


type alias View msg =
    { title : Maybe String
    , body : List (Html msg)
    }
