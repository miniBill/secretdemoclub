module Route exposing (Route(..), parse, toString)

import AppUrl exposing (AppUrl)
import Dict
import Url exposing (Url)
import Url.Builder


type Route
    = Index
    | Demos (Maybe Int)
    | Logout


parse : Url -> { route : Route, search : String }
parse url =
    let
        appUrl : AppUrl
        appUrl =
            AppUrl.fromUrl url
    in
    { route =
        case appUrl.path of
            [ "demos", year ] ->
                Demos (String.toInt year)

            [ "demos" ] ->
                Demos Nothing

            [ "logout" ] ->
                Logout

            _ ->
                Index
    , search =
        appUrl.queryParameters
            |> Dict.get "search"
            |> Maybe.withDefault []
            |> String.join " "
    }


toString : { a | search : String, route : Route } -> String
toString { search, route } =
    let
        path : List String
        path =
            case route of
                Index ->
                    []

                Demos (Just year) ->
                    [ "demos", String.fromInt year ]

                Demos Nothing ->
                    [ "demos" ]

                Logout ->
                    [ "logout" ]

        query : List (Maybe Url.Builder.QueryParameter)
        query =
            [ if String.isEmpty search then
                Nothing

              else
                Just (Url.Builder.string "search" search)
            ]
    in
    Url.Builder.absolute path (List.filterMap identity query)
