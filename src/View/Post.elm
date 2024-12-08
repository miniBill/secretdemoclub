module View.Post exposing (isMatch, view)

import Html exposing (Html)
import Html.Attributes
import Rss exposing (Post, Title(..))


view : { showKind : Bool } -> Post -> Html msg
view config post =
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
                    [ let
                        title : String
                        title =
                            Rss.titleToString post.title
                      in
                      (if config.showKind || String.isEmpty title then
                        let
                            number : String
                            number =
                                toNumber post
                        in
                        toKind post
                            ++ (if String.isEmpty number then
                                    ""

                                else
                                    " " ++ number
                               )
                            ++ (if String.isEmpty title then
                                    ""

                                else
                                    " - " ++ title
                               )

                       else
                        title
                      )
                        |> Html.text
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


toKind : Post -> String
toKind post =
    case post.title of
        Demo _ _ ->
            "Demo"

        VoiceMemo _ ->
            "Voice memo"

        BonusDemo _ ->
            "Bonus demo"

        SongIdea _ ->
            "Song idea"

        Podcast _ _ ->
            "Podcast"

        AnIdeaADay _ _ ->
            "An idea a day"

        FirstDraftFebruary _ _ ->
            "First draft February"

        AudioDiary _ _ ->
            "Audio diary"

        Other _ ->
            "Other"


toNumber : Post -> String
toNumber post =
    case post.title of
        Demo number _ ->
            number
                |> Maybe.map String.fromFloat
                |> Maybe.withDefault ""

        VoiceMemo _ ->
            ""

        BonusDemo _ ->
            ""

        SongIdea _ ->
            ""

        Podcast number _ ->
            number
                |> Maybe.map String.fromInt
                |> Maybe.withDefault ""

        AnIdeaADay number _ ->
            case number of
                Ok n ->
                    String.fromInt n

                Err e ->
                    e

        FirstDraftFebruary number _ ->
            number
                |> Maybe.map String.fromInt
                |> Maybe.withDefault ""

        AudioDiary n _ ->
            n
                |> Maybe.withDefault ""

        Other _ ->
            ""


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
