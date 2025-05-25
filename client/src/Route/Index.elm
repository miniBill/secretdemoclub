module Route.Index exposing (view)

import Html exposing (Html)
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
                    |> List.map viewYear
                    |> (::) everythingBox
                    |> Html.div
                        [ Html.Attributes.style "display" "flex"
                        , Html.Attributes.style "flex-wrap" "wrap"
                        , Html.Attributes.style "gap" "8px"
                        ]
        ]
    }


viewYear : Int -> Html msg
viewYear year =
    yearBox
        [ Html.text (String.fromInt year)
        , Route.link
            { search = ""
            , route = Route.Demos (Just year)
            }
            []
            [ Html.text "Demos" ]
        ]


everythingBox : Html msg
everythingBox =
    yearBox
        [ Html.text "All"
        , Route.link
            { search = ""
            , route = Route.Demos Nothing
            }
            []
            [ Html.text "Demos" ]
        , Route.link
            { search = " "
            , route = Route.Index
            }
            []
            [ Html.text "Everything" ]
        ]


yearBox : List (Html msg) -> Html msg
yearBox children =
    Html.div
        [ Html.Attributes.style "display" "flex"
        , Html.Attributes.style "flex-direction" "column"
        , Html.Attributes.style "gap" "8px"
        , Html.Attributes.style "padding" "8px"
        , Html.Attributes.style "border" "1px solid var(--foreground)"
        , Html.Attributes.style "border-radius" "8px"
        ]
        children
