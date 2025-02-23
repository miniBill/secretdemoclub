module Post exposing (..)

import Time
import Url exposing (Url)


type alias Post =
    { title : String
    , category : String
    , date : Time.Posix
    , image : Url
    , link : Url
    , media : Url
    , number : Maybe String
    }
