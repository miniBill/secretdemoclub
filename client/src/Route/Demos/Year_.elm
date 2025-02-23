module Route.Demos.Year_ exposing (view)

import Html
import Html.Attributes
import Html.Events
import Post exposing (Post)
import Time
import Url exposing (Url)
import View.Post


view :
    { messages | play : Url -> msg, search : String -> a }
    ->
        { model
            | posts : List Post
            , search : String
            , time : Maybe ( Time.Zone, Time.Posix )
        }
    -> Int
    -> { title : String, body : List (Html.Html msg), toolbar : List (Html.Html a) }
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
    , toolbar =
        [ Html.label []
            [ Html.text "Search "
            , Html.input
                [ Html.Attributes.type_ "search"
                , Html.Attributes.value model.search
                , Html.Events.onInput messages.search
                ]
                []
            ]
        ]
    }
