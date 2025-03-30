module Route.Index exposing (view)

import Html
import Html.Attributes
import List.Extra
import Post exposing (Post)
import RemoteData exposing (RemoteData)
import Route
import Time
import View exposing (View)


view :
    { model
        | posts : RemoteData err (List Post)
        , time : Maybe ( Time.Zone, Time.Posix )
    }
    -> View msg
view model =
    { title = ""
    , body =
        [ case model.time of
            Nothing ->
                Html.text "Loading..."

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
                    |> List.map Just
                    |> (::) Nothing
                    |> List.map
                        (\maybeYear ->
                            Html.li
                                [ Html.Attributes.style "display" "flex"
                                , Html.Attributes.style "flex-direction" "column"
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
                                , Html.a
                                    [ Html.Attributes.href
                                        (Route.toString
                                            { search = ""
                                            , route = Route.Demos maybeYear
                                            }
                                        )
                                    ]
                                    [ Html.text "Demos" ]
                                ]
                        )
                    |> Html.ul
                        [ Html.Attributes.style "display" "flex"
                        , Html.Attributes.style "flex-wrap" "wrap"
                        , Html.Attributes.style "gap" "8px"
                        ]
        ]
    }
