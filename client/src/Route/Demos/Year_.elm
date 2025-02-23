module Route.Demos.Year_ exposing (view)

import Html exposing (Html)
import Post exposing (Post)
import Time
import Url exposing (Url)
import View.Post


view :
    { messages | play : Url -> msg }
    ->
        { model
            | posts : List Post
            , search : String
            , time : Maybe ( Time.Zone, Time.Posix )
        }
    -> Int
    -> { title : String, body : List (Html.Html msg), toolbar : List (Html msg) }
view messages model year =
    let
        here =
            Maybe.map Tuple.first model.time
                |> Maybe.withDefault Time.utc
    in
    { title = String.fromInt year ++ "'s demos"
    , body =
        [ model.posts
            |> List.filter
                (\post ->
                    (post.category == "Demo")
                        && (Time.toYear here post.date == year)
                        && View.Post.isMatch model.search post
                )
            |> View.Post.viewList messages model
        ]
    }
