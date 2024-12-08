module View.Demo exposing (isMatch, view)

import Html exposing (Html)
import Html.Attributes
import Rss exposing (Post, Title(..))


view : Maybe Float -> String -> Post -> Html msg
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


isMatch : String -> Post -> Bool
isMatch needle post =
    String.isEmpty (String.trim needle)
        || (let
                haystack =
                    case post.title of
                        Demo _ title ->
                            title

                        VoiceMemo title ->
                            title

                        BonusDemo title ->
                            title

                        SongIdea title ->
                            title

                        Podcast _ title ->
                            title

                        AnIdeaADay _ title ->
                            title

                        FirstDraftFebruary _ title ->
                            title

                        AudioDiary _ title ->
                            title

                        Other title ->
                            title
            in
            String.contains (String.toLower (String.trim needle)) (String.toLower (String.trim haystack))
           )
