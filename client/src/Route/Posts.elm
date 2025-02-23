module Route.Posts exposing (view)

import Date
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Html.Keyed
import Post exposing (Post)
import Route exposing (Route(..))
import Time
import Url
import View exposing (View)


view :
    { messages | play : String -> msg }
    ->
        { model
            | time : Maybe ( Time.Zone, Time.Posix )
            , route : Route
            , search : String
        }
    -> List Post
    -> View msg
view messages model posts =
    let
        here : Time.Zone
        here =
            Maybe.map Tuple.first model.time
                |> Maybe.withDefault Time.utc

        filter : Post -> Bool
        filter post =
            case model.route of
                Index ->
                    True

                Demos maybeYear ->
                    (post.category == "Demo")
                        && (case maybeYear of
                                Nothing ->
                                    True

                                Just year ->
                                    Time.toYear here post.date == year
                           )
    in
    { title = ""
    , body =
        [ posts
            |> List.filter
                (\post ->
                    filter post && isMatch model.search post
                )
            |> viewList messages model
        ]
    }


viewList :
    { messages | play : String -> msg }
    -> { model | time : Maybe ( Time.Zone, Time.Posix ) }
    -> List Post
    -> Html msg
viewList messages model posts =
    posts
        |> List.map (\post -> ( Url.toString post.link, viewPost messages model post ))
        |> Html.Keyed.node "div"
            [ Html.Attributes.style "display" "flex"
            , Html.Attributes.style "flex-wrap" "wrap"
            , Html.Attributes.style "gap" "8px"
            , Html.Attributes.style "align-items" "stretch"
            ]


viewPost :
    { messages | play : String -> msg }
    -> { model | time : Maybe ( Time.Zone, Time.Posix ) }
    -> Post
    -> Html msg
viewPost { play } model post =
    Html.div
        [ Html.Attributes.style "max-width" "300px"
        , Html.Attributes.style "display" "flex"
        , Html.Attributes.style "flex-direction" "column"
        , Html.Attributes.style "gap" "8px"
        , Html.Attributes.style "position" "relative"
        , Html.Attributes.style "color" "white"
        , Html.Events.onClick (play post.media)
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
            [ case post.number of
                Nothing ->
                    Html.text post.category

                Just number ->
                    Html.text (post.category ++ " " ++ number)
            ]
        , Html.div
            [ Html.Attributes.style "position" "absolute"
            , Html.Attributes.style "top" "32px"
            , Html.Attributes.style "left" "32px"
            , Html.Attributes.style "right" "32px"
            , Html.Attributes.style "bottom" "32px"
            , Html.Attributes.style "border-radius" "9999px"
            , Html.Attributes.class "show-on-parent-hover"
            , Html.Attributes.style "background-color" "#0008"
            ]
            [ Html.div
                [ Html.Attributes.style "position" "absolute"
                , Html.Attributes.style "top" "50%"
                , Html.Attributes.style "left" "50%"
                , Html.Attributes.style "transform" "translate(-50%,-50%)"
                , Html.Attributes.style "width" "0"
                , Html.Attributes.style "height" "0"
                , Html.Attributes.style "border-top" "48px solid transparent"
                , Html.Attributes.style "border-bottom" "48px solid transparent"
                , Html.Attributes.style "border-left" "78px solid #fff4"
                ]
                []
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
            [ Html.text post.title
            ]
        , Html.a
            [ Html.Attributes.style "position" "absolute"
            , Html.Attributes.style "bottom" "0"
            , Html.Attributes.style "left" "0"
            , Html.Attributes.style "padding" "8px"
            , Html.Attributes.style "color" "oklch(70.71% 0.1512 264.05300810418345)"
            , Html.Attributes.classList
                [ ( "show-on-parent-hover", True )
                , ( "show-if-hover-none", True )
                ]
            , Html.Attributes.href (Url.toString post.link)
            , Html.Attributes.style "flex" "1"
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
            [ Html.Attributes.style "position" "absolute"
            , Html.Attributes.style "bottom" "0"
            , Html.Attributes.style "right" "0"
            , Html.Attributes.style "padding" "8px"
            , Html.Attributes.style "color" "oklch(70.71% 0.1512 264.05300810418345)"
            , Html.Attributes.classList
                [ ( "show-on-parent-hover", True )
                , ( "show-if-hover-none", True )
                ]
            , Html.Attributes.href ("/media/" ++ post.media)
            ]
            [ Html.text "Download" ]
        , Html.img
            [ Html.Attributes.src ("/media/" ++ post.image)
            , Html.Attributes.style "width" "100%"
            ]
            []
        ]


isMatch : String -> Post -> Bool
isMatch needle post =
    let
        cleanNeedle : String
        cleanNeedle =
            needle
                |> String.trim
                |> String.toLower
    in
    String.isEmpty cleanNeedle
        || String.contains cleanNeedle (String.toLower (String.trim post.title))
