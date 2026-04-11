module Route.Posts exposing (view)

import Date
import Filter exposing (Filter(..))
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events
import Html.Keyed
import Json.Encode
import List.Extra
import Post exposing (Post)
import Theme
import Time
import Types exposing (Theme)
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
    in
    { title = Nothing
    , content =
        ( []
        , [ if model.hasServiceWorker then
                let
                    files : String
                    files =
                        posts
                            |> List.filter (Post.isMatch (Filter.current model.filter) here)
                            |> List.filterMap (postToDownloadData model.root)
                            |> Json.Encode.list identity
                            |> Json.Encode.encode 0
                in
                Theme.linkButton
                    [ HA.download "sdc-download.zip" ]
                    { theme = model.theme
                    , text = "Download all"
                    , href = Url.Builder.absolute [ "download" ] [ Url.Builder.string "files" files ]
                    }

            else
                Html.text ""
          , posts
                |> List.sortBy (\post -> Time.posixToMillis post.date |> negate)
                |> viewList messages model here
          ]
        )
    }


postToDownloadData : Url -> Post -> Maybe Json.Encode.Value
postToDownloadData root post =
    post.media
        |> Maybe.map
            (\media ->
                let
                    extension : String
                    extension =
                        media
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

                    mediaUrl : String
                    mediaUrl =
                        Url.Builder.crossOrigin (Url.toString root) [ "media", media ] []
                in
                Json.Encode.object
                    [ ( "filename", Json.Encode.string filename )
                    , ( "url", Json.Encode.string mediaUrl )
                    , ( "mtime", Json.Encode.int (Time.posixToMillis post.date) )
                    ]
            )


viewList :
    { messages | play : String -> msg }
    -> { model | filter : Filter }
    -> Time.Zone
    -> List Post
    -> Html msg
viewList messages model here posts =
    posts
        |> List.map (\post -> ( Url.toString post.link, viewPost messages model here post ))
        |> Html.Keyed.node "div"
            [ HA.style "display" "flex"
            , HA.style "flex-wrap" "wrap"
            , HA.style "align-items" "stretch"
            , HA.style "justify-content" "center"
            ]


type FilterStatus
    = Matches
    | MatchesPrevious
    | DoesntMatch


viewPost :
    { messages | play : String -> msg }
    -> { model | filter : Filter }
    -> Time.Zone
    -> Post
    -> Html msg
viewPost { play } model here post =
    let
        filterStatus : FilterStatus
        filterStatus =
            getFilterStatus model.filter here post

        commonAttributes : List (Html.Attribute msg)
        commonAttributes =
            [ HA.style "width" "40vmin"
            , HA.style "height" "40vmin"
            , HA.style "max-width" "300px"
            , HA.style "max-height" "300px"
            , HA.style "position" "relative"
            , HA.style "color" "var(--offwhite)"
            , HA.style "font-size" "calc(min(2.2vmin, 17.5px))"
            ]

        perStateAttributes : List (Html.Attribute msg)
        perStateAttributes =
            case filterStatus of
                Matches ->
                    []

                MatchesPrevious ->
                    [ HA.style "filter" "grayscale(1)"
                    ]

                DoesntMatch ->
                    [ HA.style "display" "none"
                    , HA.style "filter" "grayscale(1)"
                    ]

        perMediaAttributes : List (Html.Attribute msg)
        perMediaAttributes =
            case post.media of
                Just media ->
                    [ Html.Events.onClick (play media) ]

                Nothing ->
                    []
    in
    Theme.column
        (commonAttributes
            ++ perStateAttributes
            ++ perMediaAttributes
        )
        [ Html.div
            [ HA.style "position" "absolute"
            , HA.style "top" "0"
            , HA.style "left" "0"
            , HA.style "bottom" "0"
            , HA.style "right" "0"
            , HA.style "background-color" "#000a"
            ]
            []
        , viewPostCategory post
        , viewPostPlayHover post
        , viewPostTitle post
        , viewPostDate post here
        , viewPostDownloadButton post
        , Html.img
            [ HA.src ("/media/" ++ post.image)
            , HA.style "width" "100%"
            ]
            []
        ]


viewPostCategory : Post -> Html msg
viewPostCategory post =
    Html.div
        [ HA.style "position" "absolute"
        , HA.style "top" "0"
        , HA.style "left" "0"
        , HA.style "text-align" "center"
        , HA.style "width" "100%"
        , HA.style "padding" "8px 8px 24px 8px"
        , HA.style "font-size" "1.2em"
        , HA.style "font-weight" "semibold"
        , HA.style "flex" "1"
        ]
        [ case post.number of
            Nothing ->
                Html.text post.category

            Just number ->
                Html.text (post.category ++ " " ++ number)
        ]


viewPostPlayHover : Post -> Html msg
viewPostPlayHover post =
    case post.media of
        Nothing ->
            Html.text ""

        Just _ ->
            Html.div
                [ HA.style "position" "absolute"
                , HA.style "top" "32px"
                , HA.style "left" "32px"
                , HA.style "right" "28px"
                , HA.style "bottom" "28px"
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


viewPostTitle : Post -> Html msg
viewPostTitle post =
    Html.div
        [ HA.style "position" "absolute"
        , HA.style "top" "50%"
        , HA.style "left" "50%"
        , HA.style "transform" "translate(-50%,-50%)"
        , Theme.padding
        , HA.style "width" "100%"
        , HA.style "font-size" "2em"
        , HA.style "font-weight" "semibold"
        , HA.style "text-align" "center"
        , HA.style "overflow-wrap" "break-word"
        ]
        [ Html.text post.title
        ]


viewPostDate : Post -> Time.Zone -> Html msg
viewPostDate post here =
    Html.a
        [ HA.style "position" "absolute"
        , HA.style "bottom" "0"
        , HA.style "left" "0"
        , HA.style "text-align" "center"
        , case post.media of
            Nothing ->
                HA.style "width" "100%"

            Just _ ->
                HA.style "" ""
        , Theme.padding
        , HA.classList
            [ ( "show-on-parent-hover", True )
            , ( "show-if-hover-none", True )
            ]
        , HA.href (Url.toString post.link)
        , HA.style "flex" "1"
        ]
        [ Date.fromPosix here post.date
            |> Date.toIsoString
            |> Html.text
        ]


viewPostDownloadButton : Post -> Html msg
viewPostDownloadButton post =
    case post.media of
        Nothing ->
            Html.text ""

        Just media ->
            Html.a
                [ HA.style "position" "absolute"
                , HA.style "bottom" "0"
                , HA.style "right" "0"
                , Theme.padding
                , HA.classList
                    [ ( "show-on-parent-hover", True )
                    , ( "show-if-hover-none", True )
                    ]
                , HA.href ("/media/" ++ media)
                , let
                    extension : String
                    extension =
                        media
                            |> String.split "."
                            |> List.Extra.last
                            |> Maybe.withDefault "mp3"
                  in
                  HA.download (post.title ++ "." ++ extension)
                ]
                [ Html.text "Download" ]


getFilterStatus : Filter -> Time.Zone -> Post -> FilterStatus
getFilterStatus filter here post =
    case filter of
        Filtered current ->
            if Post.isMatch current here post then
                Matches

            else
                DoesntMatch

        Filtering { current, previous } ->
            if Post.isMatch current here post then
                Matches

            else if Post.isMatch previous here post then
                MatchesPrevious

            else
                DoesntMatch
