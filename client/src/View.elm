module View exposing (View)

{-|

@docs View

-}

import Html exposing (Attribute, Html)


type alias View msg =
    { title : Maybe String
    , content : ( List (Attribute msg), List (Html msg) )
    }
