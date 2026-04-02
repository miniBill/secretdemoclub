module Route exposing (Route(..), link, parse, toString)

import AppUrl exposing (AppUrl)
import Dict
import Filter exposing (Filter(..), FilterData)
import Html exposing (Html)
import Html.Attributes
import Set
import Url exposing (Url)
import Url.Builder


type Route
    = Index Filter
    | Logout


parse : Url -> Route
parse url =
    let
        appUrl : AppUrl
        appUrl =
            AppUrl.fromUrl url
    in
    case appUrl.path of
        [ "logout" ] ->
            Logout

        _ ->
            let
                emptyFilter : Filter.FilterData
                emptyFilter =
                    Filter.empty
            in
            List.foldl
                (\piece filter ->
                    case String.toInt piece of
                        Just year ->
                            { filter | year = Just year }

                        Nothing ->
                            { filter | categories = Set.insert piece filter.categories }
                )
                { emptyFilter
                    | search =
                        appUrl.queryParameters
                            |> Dict.get "search"
                            |> Maybe.withDefault []
                            |> String.join " "
                }
                appUrl.path
                |> Filtered
                |> Index


toString : Route -> String
toString route =
    let
        path : List String
        path =
            case route of
                Index filter ->
                    let
                        currentFilter : FilterData
                        currentFilter =
                            Filter.current filter
                    in
                    case currentFilter.year of
                        Just year ->
                            (currentFilter.categories |> Set.toList) ++ [ String.fromInt year ]

                        Nothing ->
                            currentFilter.categories |> Set.toList

                Logout ->
                    [ "logout" ]

        query : List (Maybe Url.Builder.QueryParameter)
        query =
            [ case route of
                Index filter ->
                    let
                        search : String
                        search =
                            (Filter.current filter).search
                    in
                    if String.isEmpty search then
                        Nothing

                    else
                        Just (Url.Builder.string "search" search)

                Logout ->
                    Nothing
            ]
    in
    Url.Builder.absolute path (List.filterMap identity query)


link : Route -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
link route attrs children =
    Html.a
        (Html.Attributes.href (toString route) :: attrs)
        children
