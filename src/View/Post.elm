module View.Post exposing (isMatch, view)

import Date
import Html exposing (Html)
import Html.Attributes
import Rss exposing (Post, Title(..))
import Shared


view : Shared.Model -> { showKind : Bool } -> Post -> Html msg
view shared config post =
    Html.div
        [ Html.Attributes.style "max-width" "300px"
        , Html.Attributes.style "display" "flex"
        , Html.Attributes.style "flex-direction" "column"
        , Html.Attributes.style "gap" "8px"
        ]
        [ Html.span
            [ Html.Attributes.style "font-size" "1.4rem"
            , Html.Attributes.style "font-weight" "semibold"
            , Html.Attributes.style "text-align" "center"
            , Html.Attributes.style "flex" "1"
            ]
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
        , Html.div
            [ Html.Attributes.style "display" "flex"
            ]
            [ Html.a
                [ Html.Attributes.href post.link
                , Html.Attributes.style "flex" "1"
                ]
                [ case shared.time of
                    Nothing ->
                        Html.text ""

                    Just ( here, _ ) ->
                        Date.fromPosix here post.pubDate
                            |> Date.toIsoString
                            |> Html.text
                ]
            , Html.a
                [ Html.Attributes.href post.mediaUrl
                ]
                [ Html.text "Download" ]
            ]
        , Html.img
            [ Html.Attributes.src post.image
            , Html.Attributes.style "width" "100%"
            ]
            []
        , Html.audio
            [ Html.Attributes.controls True
            , Html.Attributes.src post.mediaUrl
            ]
            []
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
