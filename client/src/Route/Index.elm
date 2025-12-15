module Route.Index exposing (view)

import Html exposing (Attribute, Html)
import Html.Attributes as HA
import List.Extra
import Post exposing (Post)
import RemoteData exposing (RemoteData)
import Route exposing (Filter)
import Set
import Time
import View exposing (View)


view :
    { model
        | posts : RemoteData err (List Post)
        , filter : Filter
        , time : Maybe ( Time.Zone, Time.Posix )
    }
    -> View msg
view model =
    { title = Nothing
    , content =
        case model.time of
            Nothing ->
                ( [], [ Html.text "Loading time information..." ] )

            Just ( here, now ) ->
                loadedView model here now
    }


loadedView :
    { model
        | posts : RemoteData err (List Post)
        , filter : Filter
        , time : Maybe ( Time.Zone, Time.Posix )
    }
    -> Time.Zone
    -> Time.Posix
    -> ( List (Attribute msg), List (Html msg) )
loadedView model here now =
    let
        posts : List Post
        posts =
            model.posts
                |> RemoteData.withDefault []

        lastPosted : Time.Posix
        lastPosted =
            posts
                |> List.Extra.last
                |> Maybe.map .date
                |> Maybe.withDefault now

        yearOfLastPost : Int
        yearOfLastPost =
            Time.toYear here lastPosted

        firstRowView : List (Html msg)
        firstRowView =
            viewYear here model.filter.search posts Nothing

        yearViews : List (Html msg)
        yearViews =
            List.range 2015 yearOfLastPost
                |> List.reverse
                |> List.concatMap (\year -> viewYear here model.filter.search posts (Just year))
    in
    ( [ HA.style "align-items" "center"
      , HA.style "gap" "16px"
      , HA.style "flex" "1 0"
      ]
    , [ Html.text "Click on a link below to see the content"
      , (firstRowView ++ yearViews)
            |> Html.div
                [ HA.style "display" "grid"
                , HA.style "gap" "24px 0"
                , HA.style "grid-template-columns" "auto 1fr"
                ]
      ]
    )


viewYear : Time.Zone -> String -> List Post -> Maybe Int -> List (Html msg)
viewYear here search posts maybeYear =
    let
        isCorrectYear : Post -> Bool
        isCorrectYear post =
            case maybeYear of
                Nothing ->
                    True

                Just year ->
                    Time.toYear here post.date == year

        categories : List String
        categories =
            posts
                |> List.filterMap
                    (\post ->
                        if isCorrectYear post then
                            Just post.category

                        else
                            Nothing
                    )
                |> Set.fromList
                |> Set.toList
                |> List.sortBy categoryOrder

        categoryLink : String -> Html msg
        categoryLink category =
            Route.link
                (Route.Index
                    { categories = Set.singleton (String.toLower category)
                    , year = maybeYear
                    , search = search
                    }
                )
                []
                [ Html.text category ]
    in
    [ Html.div
        [ HA.style "padding" "0 24px 8px 0"
        , HA.style "border-bottom" "1px solid var(--offwhite)"
        ]
        [ Route.link
            (Route.Index
                { categories = Set.singleton "all"
                , year = maybeYear
                , search = search
                }
            )
            []
            [ case maybeYear of
                Nothing ->
                    Html.text "All"

                Just year ->
                    Html.text (String.fromInt year)
            ]
        ]
    , Html.div
        [ HA.style "display" "flex"
        , HA.style "gap" "8px 24px"
        , HA.style "padding" "0 0 8px 0"
        , HA.style "border-bottom" "1px solid var(--offwhite)"
        , HA.style "flex-wrap" "wrap"
        ]
        (List.map categoryLink
            (if List.isEmpty categories then
                [ "Demos", "Voice memos", "Song ideas", "Bonus demos", "Others" ]

             else
                categories
            )
        )
    ]


categoryOrder : String -> ( number, String )
categoryOrder c =
    case c of
        "Demos" ->
            ( 1, c )

        "Voice memos" ->
            ( 2, c )

        "Song ideas" ->
            ( 3, c )

        "Bonus demos" ->
            ( 5, c )

        "Others" ->
            ( 6, c )

        _ ->
            ( 10, c )
