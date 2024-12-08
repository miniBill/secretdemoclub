module View.Post exposing (isMatch, view, viewList)

import Date
import Html exposing (Html)
import Html.Attributes
import Rss exposing (Post, Title(..))
import Shared


view : Shared.Model -> Post -> Html msg
view shared post =
    Html.div
        [ Html.Attributes.style "max-width" "300px"
        , Html.Attributes.style "display" "flex"
        , Html.Attributes.style "flex-direction" "column"
        , Html.Attributes.style "gap" "8px"
        ]
        [ Html.div
            [ Html.Attributes.style "position" "relative"
            , Html.Attributes.style "color" "white"
            ]
            [ Html.div
                [ Html.Attributes.style "position" "absolute"
                , Html.Attributes.style "top" "0"
                , Html.Attributes.style "left" "0"
                , Html.Attributes.style "bottom" "0"
                , Html.Attributes.style "right" "0"
                , Html.Attributes.style "background-color" "#0008"
                ]
                []
            , Html.div
                [ Html.Attributes.style "position" "absolute"
                , Html.Attributes.style "top" "0"
                , Html.Attributes.style "left" "0"
                , Html.Attributes.style "padding" "8px 8px 24px 8px"
                , Html.Attributes.style "width" "100%"
                , Html.Attributes.style "font-size" "1.2rem"
                , Html.Attributes.style "font-weight" "semibold"
                , Html.Attributes.style "text-align" "center"
                , Html.Attributes.style "flex" "1"
                ]
                [ let
                    number : String
                    number =
                        toNumber post
                  in
                  if String.isEmpty number then
                    Html.text (toKind post)

                  else
                    Html.text (toKind post ++ " " ++ number)
                ]
            , Html.div
                [ Html.Attributes.style "position" "absolute"
                , Html.Attributes.style "top" "50%"
                , Html.Attributes.style "left" "50%"
                , Html.Attributes.style "transform" "translate(-50%,-50%)"
                , Html.Attributes.style "padding" "8px"
                , Html.Attributes.style "width" "100%"
                , Html.Attributes.style "font-size" "2rem"
                , Html.Attributes.style "font-weight" "semibold"
                , Html.Attributes.style "text-align" "center"
                ]
                [ Html.text (Rss.titleToString post.title)
                ]
            , Html.a
                [ Html.Attributes.style "position" "absolute"
                , Html.Attributes.style "bottom" "0"
                , Html.Attributes.style "left" "0"
                , Html.Attributes.style "padding" "8px"
                , Html.Attributes.style "color" "oklch(70.71% 0.1512 264.05300810418345)"
                , Html.Attributes.class "show-on-parent-hover"
                , Html.Attributes.href post.link
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
                [ Html.Attributes.style "position" "absolute"
                , Html.Attributes.style "bottom" "0"
                , Html.Attributes.style "right" "0"
                , Html.Attributes.style "padding" "8px"
                , Html.Attributes.style "color" "oklch(70.71% 0.1512 264.05300810418345)"
                , Html.Attributes.class "show-on-parent-hover"
                , Html.Attributes.href post.mediaUrl
                ]
                [ Html.text "Download" ]
            , Html.img
                [ Html.Attributes.src post.image
                , Html.Attributes.style "width" "100%"
                ]
                []
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


viewList : Shared.Model -> List Post -> Html.Html msg
viewList shared posts =
    posts
        |> List.map (view shared)
        |> Html.div
            [ Html.Attributes.style "display" "flex"
            , Html.Attributes.style "flex-wrap" "wrap"
            , Html.Attributes.style "gap" "8px"
            , Html.Attributes.style "align-items" "stretch"
            ]
