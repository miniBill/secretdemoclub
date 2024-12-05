module View.Demo exposing (view)

import Html exposing (Html)
import Html.Attributes
import Rss


view : Maybe Float -> String -> Rss.Post -> Html msg
view number title post =
    Html.div []
        [ Html.a
            [ Html.Attributes.href post.link ]
            [ Html.h2 []
                [ case number of
                    Just n ->
                        Html.text ("#" ++ String.fromFloat n ++ " ")

                    Nothing ->
                        Html.text ""
                , Html.text title
                ]
            ]
        , Html.div []
            [ Html.audio
                [ Html.Attributes.controls True
                , Html.Attributes.src post.mediaUrl
                ]
                []
            , Html.br [] []
            , Html.a [ Html.Attributes.href post.mediaUrl ] [ Html.text "Download" ]
            ]
        ]
