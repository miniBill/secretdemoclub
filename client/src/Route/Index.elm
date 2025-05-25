module Route.Index exposing (view)

import Html exposing (Html)
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
    , body =
        [ case model.time of
            Nothing ->
                Html.text "Loading time information..."

            Just ( here, now ) ->
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
                in
                List.range 2015 yearOfLastPost
                    |> List.reverse
                    |> List.map (\year -> viewYear here model.filter.search posts (Just year))
                    |> (::) (viewYear here model.filter.search posts Nothing)
                    |> Html.div
                        [ HA.style "display" "flex"
                        , HA.style "flex-direction" "column"
                        , HA.style "gap" "8px"
                        ]
        ]
    }


viewYear : Time.Zone -> String -> List Post -> Maybe Int -> Html msg
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
                |> List.sortBy
                    (\c ->
                        case c of
                            "Demo" ->
                                ( 80, c )

                            "Other" ->
                                ( 90, c )

                            _ ->
                                ( 10, c )
                    )

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
                [ Html.text
                    (if String.endsWith "emo" category then
                        category ++ "s"

                     else
                        category
                    )
                ]
    in
    Html.div
        [ HA.style "display" "flex"
        , HA.style "gap" "8px"
        , HA.style "padding" "8px"
        , HA.style "border" "1px solid var(--offwhite)"
        , HA.style "border-radius" "8px"
        , HA.style "justify-content" "space-between"
        ]
        [ Route.link
            (Route.Index
                { categories = Set.empty
                , year = maybeYear
                , search =
                    if maybeYear == Nothing && String.isEmpty search then
                        " "

                    else
                        search
                }
            )
            []
            [ case maybeYear of
                Nothing ->
                    Html.text "All"

                Just year ->
                    Html.text (String.fromInt year)
            ]
        , Html.div
            [ HA.style "display" "flex"
            , HA.style "gap" "8px"
            , HA.style "flex-wrap" "wrap"
            ]
            (List.map categoryLink categories)
        ]
