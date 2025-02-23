module Route.Index exposing (view)

import Html
import Html.Attributes
import List.Extra
import Maybe.Extra
import Post exposing (Post)
import RemoteData exposing (RemoteData)
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
        [ Html.a
            [ Html.Attributes.href "/demos"
            ]
            [ Html.text "Demos" ]
        , let
            lastPosted : Maybe Time.Posix
            lastPosted =
                model.posts
                    |> RemoteData.withDefault []
                    |> List.Extra.last
                    |> Maybe.map .date
                    |> Maybe.Extra.orElseLazy (\_ -> Maybe.map Tuple.second model.time)

            here : Time.Zone
            here =
                model.time
                    |> Maybe.map Tuple.first
                    |> Maybe.withDefault Time.utc

            maybeLastYear : Maybe Int
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
    }
