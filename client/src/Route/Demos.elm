module Route.Demos exposing (view)

import Post exposing (Post)
import Time
import Url exposing (Url)
import View exposing (View)
import View.Post


view :
    { messages
        | play : Url -> msg
    }
    ->
        { model
            | posts : List Post
            , search : String
            , time : Maybe ( Time.Zone, Time.Posix )
        }
    -> View msg
view messages model =
    { title = "demos"
    , body =
        [ model.posts
            |> List.filterMap
                (\post ->
                    if
                        (post.category == "Demo")
                            && View.Post.isMatch model.search post
                    then
                        Just post

                    else
                        Nothing
                )
            |> View.Post.viewList messages model
        ]
    }
