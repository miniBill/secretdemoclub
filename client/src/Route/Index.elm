module Route.Index exposing (view)

import Html exposing (Html)
import Html.Attributes
import Html.Events
import List.Extra
import Maybe.Extra
import Post exposing (Post)
import Time
import Url exposing (Url)
import View exposing (View)
import View.Post


view :
    { messages
        | search : String -> msg
        , play : Url -> msg
    }
    ->
        { model
            | search : String
            , posts : List Post
            , time : Maybe ( Time.Zone, Time.Posix )
        }
    -> View msg
view messages model =
    let
        toolbar : List (Html msg)
        toolbar =
            [ Html.label []
                [ Html.text "Search "
                , Html.input
                    [ Html.Attributes.type_ "search"
                    , Html.Attributes.value model.search
                    , Html.Events.onInput messages.search
                    ]
                    []
                ]
            ]
    in
    if String.isEmpty model.search || List.isEmpty model.posts then
        { title = ""
        , body =
            [ Html.a
                [ Html.Attributes.href "/demos"
                ]
                [ Html.text "Demos" ]
            , let
                lastPosted =
                    model.posts
                        |> List.Extra.last
                        |> Maybe.map .date
                        |> Maybe.Extra.orElseLazy (\_ -> Maybe.map Tuple.second model.time)

                here =
                    model.time
                        |> Maybe.map Tuple.first
                        |> Maybe.withDefault Time.utc

                maybeLastYear =
                    Maybe.map (Time.toYear here) lastPosted
              in
              case maybeLastYear of
                Nothing ->
                    Html.text ""

                Just lastYear ->
                    List.range 2015 lastYear
                        |> List.map
                            (\year ->
                                Html.li []
                                    [ Html.a
                                        [ Html.Attributes.href
                                            ("/demos/" ++ String.fromInt year)
                                        ]
                                        [ Html.text (String.fromInt year) ]
                                    ]
                            )
                        |> Html.ul []
            ]
        , toolbar = toolbar
        }

    else
        { title = ""
        , body =
            [ model.posts
                |> List.filterMap
                    (\post ->
                        if View.Post.isMatch model.search post then
                            Just post

                        else
                            Nothing
                    )
                |> View.Post.viewList messages model
            ]
        , toolbar = toolbar
        }
