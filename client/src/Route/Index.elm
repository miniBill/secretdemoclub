module Route.Index exposing (view)

import Html exposing (Html)
import Html.Attributes
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
                    lastPosted : Time.Posix
                    lastPosted =
                        model.posts
                            |> RemoteData.withDefault []
                            |> List.Extra.last
                            |> Maybe.map .date
                            |> Maybe.withDefault now

                    yearOfLastPost : Int
                    yearOfLastPost =
                        Time.toYear here lastPosted
                in
                List.range 2015 yearOfLastPost
                    |> List.map (\year -> viewYear model.filter.search (Just year))
                    |> (::) (viewYear model.filter.search Nothing)
                    |> Html.div
                        [ Html.Attributes.style "display" "flex"
                        , Html.Attributes.style "flex-direction" "column"
                        , Html.Attributes.style "gap" "8px"
                        ]
        ]
    }


viewYear : String -> Maybe Int -> Html msg
viewYear search maybeYear =
    Html.div
        [ Html.Attributes.style "display" "flex"
        , Html.Attributes.style "gap" "8px"
        , Html.Attributes.style "padding" "8px"
        , Html.Attributes.style "border" "1px solid var(--foreground)"
        , Html.Attributes.style "border-radius" "8px"
        ]
        [ case maybeYear of
            Nothing ->
                Html.text "All"

            Just year ->
                Html.text (String.fromInt year)
        , Html.div
            [ Html.Attributes.style "flex" "1"
            ]
            []
        , Route.link
            (Route.Index
                { categories = Set.singleton "demos"
                , year = maybeYear
                , search = search
                }
            )
            []
            [ Html.text "Demos" ]
        , Route.link
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
            [ Html.text "Everything" ]
        ]
