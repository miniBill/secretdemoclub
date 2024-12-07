module View.Demo exposing (view)

import Html exposing (Html)
import Html.Attributes
import Rss


view : Maybe Float -> String -> Rss.Post -> Html msg
view number title post =
    Html.div
        [ Html.Attributes.style "max-width" "300px"
        , Html.Attributes.style "display" "flex"
        , Html.Attributes.style "flex-direction" "column"
        , Html.Attributes.style "gap" "8px"
        ]
        [ Html.div [ Html.Attributes.style "flex" "1" ]
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
            ]
        , Html.img
            [ Html.Attributes.src post.image
            , Html.Attributes.style "width" "100%"
            ]
            []
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
