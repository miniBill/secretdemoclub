module Route exposing (Filter, Route(..), emptyFilter, index, link, parse, toString)

import AppUrl exposing (AppUrl)
import Dict
import Html exposing (Html)
import Html.Attributes
import Set exposing (Set)
import Url exposing (Url)
import Url.Builder


type Route
    = Index Filter
    | Logout


type alias Filter =
    { year : Maybe Int
    , categories : Set String
    , search : String
    }


emptyFilter : Filter
emptyFilter =
    { year = Nothing
    , categories = Set.empty
    , search = ""
    }


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
                |> Index


toString : Route -> String
toString route =
    let
        path : List String
        path =
            case route of
                Index filter ->
                    case filter.year of
                        Just year ->
                            (filter.categories |> Set.toList) ++ [ String.fromInt year ]

                        Nothing ->
                            filter.categories |> Set.toList

                Logout ->
                    [ "logout" ]

        query : List (Maybe Url.Builder.QueryParameter)
        query =
            [ case route of
                Index { search } ->
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


index : Route
index =
    Index emptyFilter
