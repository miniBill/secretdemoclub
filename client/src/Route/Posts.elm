module Route.Posts exposing (view)

import Date
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events
import Html.Keyed
import Json.Encode
import List.Extra
import Post exposing (Post)
import Route exposing (Filter)
import Time
import Types exposing (Theme(..))
import Url exposing (Url)
import Url.Builder
import View exposing (View)


view :
    { messages
        | play : String -> msg
    }
    ->
        { model
            | time : Maybe ( Time.Zone, Time.Posix )
            , filter : Filter
            , hasServiceWorker : Bool
            , root : Url
            , theme : Theme
        }
    -> List Post
    -> View msg
view messages model posts =
    let
        here : Time.Zone
        here =
            Maybe.map Tuple.first model.time
                |> Maybe.withDefault Time.utc

        filteredPosts : List Post
        filteredPosts =
            List.filter (Post.isMatch model.filter here) posts
    in
    { title = Nothing
    , content =
        Html.div
            [ HA.style "display" "flex"
            , HA.style "flex-direction" "column"
            , HA.style "gap" "8px"
            , HA.style "align-items" "center"
            ]
            [ if model.hasServiceWorker then
                let
                    files : String
                    files =
                        filteredPosts
                            |> Json.Encode.list (postToDownloadData model.root)
                            |> Json.Encode.encode 0
                in
                Html.a
                    [ HA.href
                        (Url.Builder.absolute
                            [ "download" ]
                            [ Url.Builder.string "files" files ]
                        )
                    , HA.download "sdc-download.zip"
                    ]
                    [ Html.div
                        [ HA.style "padding" "8px"
                        , case model.theme of
                            Dark ->
                                HA.style "background" "var(--red)"

                            Light ->
                                HA.style "background" "var(--navy)"
                        , HA.style "color" "var(--offwhite)"
                        , HA.style "border-radius" "999px"
                        ]
                        [ Html.text "Download all"
                        ]
                    ]

              else
                Html.text ""
            , filteredPosts
                |> List.sortBy (\post -> Time.posixToMillis post.date |> negate)
                |> viewList messages model
            ]
    }


postToDownloadData : Url -> Post -> Json.Encode.Value
postToDownloadData root post =
    let
        extension : String
        extension =
            post.media
                |> String.split "."
                |> List.Extra.last
                |> Maybe.withDefault "mp3"

        filename : String
        filename =
            post.category
                ++ "/"
                ++ (case post.number of
                        Nothing ->
                            ""

                        Just n ->
                            String.replace "/" "_" n ++ " - "
                   )
                ++ String.replace "/" "_" post.title
                ++ "."
                ++ extension

        media : String
        media =
            Url.Builder.crossOrigin (Url.toString root) [ "media", post.media ] []
    in
    Json.Encode.object
        [ ( "filename", Json.Encode.string filename )
        , ( "url", Json.Encode.string media )
        , ( "mtime", Json.Encode.int (Time.posixToMillis post.date) )
        ]


viewList :
    { messages | play : String -> msg }
    -> { model | time : Maybe ( Time.Zone, Time.Posix ) }
    -> List Post
    -> Html msg
viewList messages model posts =
    posts
        |> List.map (\post -> ( Url.toString post.link, viewPost messages model post ))
        |> Html.Keyed.node "div"
            [ HA.style "display" "flex"
            , HA.style "flex-wrap" "wrap"
            , HA.style "gap" "8px"
            , HA.style "align-items" "stretch"
            , HA.style "justify-content" "center"
            ]


viewPost :
    { messages | play : String -> msg }
    -> { model | time : Maybe ( Time.Zone, Time.Posix ) }
    -> Post
    -> Html msg
viewPost { play } model post =
    Html.div
        [ HA.style "width" "40vmin"
        , HA.style "height" "40vmin"
        , HA.style "max-width" "300px"
        , HA.style "max-height" "300px"
        , HA.style "display" "flex"
        , HA.style "flex-direction" "column"
        , HA.style "gap" "8px"
        , HA.style "position" "relative"
        , HA.style "color" "var(--offwhite)"
        , HA.style "font-size" "calc(min(2.2vmin, 17.5px))"
        , Html.Events.onClick (play post.media)
        ]
        [ Html.div
            [ HA.style "position" "absolute"
            , HA.style "top" "0"
            , HA.style "left" "0"
            , HA.style "bottom" "0"
            , HA.style "right" "0"
            , HA.style "background-color" "#0008"
            ]
            []
        , Html.div
            [ HA.style "position" "absolute"
            , HA.style "top" "0"
            , HA.style "left" "0"
            , HA.style "padding" "8px 8px 24px 8px"
            , HA.style "width" "100%"
            , HA.style "font-size" "1.2em"
            , HA.style "font-weight" "semibold"
            , HA.style "text-align" "center"
            , HA.style "flex" "1"
            ]
            [ case post.number of
                Nothing ->
                    Html.text post.category

                Just number ->
                    Html.text (post.category ++ " " ++ number)
            ]
        , Html.div
            [ HA.style "position" "absolute"
            , HA.style "top" "32px"
            , HA.style "left" "32px"
            , HA.style "right" "32px"
            , HA.style "bottom" "32px"
            , HA.style "border-radius" "9999px"
            , HA.class "show-on-parent-hover"
            , HA.style "background-color" "#0008"
            ]
            [ Html.div
                [ HA.style "position" "absolute"
                , HA.style "top" "50%"
                , HA.style "left" "50%"
                , HA.style "transform" "translate(-50%,-50%)"
                , HA.style "width" "0"
                , HA.style "height" "0"
                , HA.style "border-top" "48px solid transparent"
                , HA.style "border-bottom" "48px solid transparent"
                , HA.style "border-left" "78px solid #fff4"
                ]
                []
            ]
        , Html.div
            [ HA.style "position" "absolute"
            , HA.style "top" "50%"
            , HA.style "left" "50%"
            , HA.style "transform" "translate(-50%,-50%)"
            , HA.style "padding" "8px"
            , HA.style "width" "100%"
            , HA.style "font-size" "2em"
            , HA.style "font-weight" "semibold"
            , HA.style "text-align" "center"
            ]
            [ Html.text post.title
            ]
        , Html.a
            [ HA.style "position" "absolute"
            , HA.style "bottom" "0"
            , HA.style "left" "0"
            , HA.style "padding" "8px"
            , HA.classList
                [ ( "show-on-parent-hover", True )
                , ( "show-if-hover-none", True )
                ]
            , HA.href (Url.toString post.link)
            , HA.style "flex" "1"
            ]
            [ case model.time of
                Nothing ->
                    Html.text ""

                Just ( here, _ ) ->
                    Date.fromPosix here post.date
                        |> Date.toIsoString
                        |> Html.text
            ]
        , Html.a
            [ HA.style "position" "absolute"
            , HA.style "bottom" "0"
            , HA.style "right" "0"
            , HA.style "padding" "8px"
            , HA.classList
                [ ( "show-on-parent-hover", True )
                , ( "show-if-hover-none", True )
                ]
            , HA.href ("/media/" ++ post.media)
            , let
                extension : String
                extension =
                    post.media
                        |> String.split "."
                        |> List.Extra.last
                        |> Maybe.withDefault "mp3"
              in
              HA.download (post.title ++ "." ++ extension)
            ]
            [ Html.text "Download" ]
        , Html.img
            [ HA.src ("/media/" ++ post.image)
            , HA.style "width" "100%"
            ]
            []
        ]
