module Shared.Model exposing (Model)

{-| -}

import Rss exposing (Post)
import Time


{-| Normally, this value would live in "Shared.elm"
but that would lead to a circular dependency import cycle.

For that reason, both `Shared.Model` and `Shared.Msg` are in their
own file, so they can be imported by `Effect.elm`

-}
type alias Model =
    { rss :
        { url : String
        , posts : List Post
        }
    , time : Maybe ( Time.Zone, Time.Posix )
    }
