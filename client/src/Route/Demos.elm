module Route.Demos exposing (Msg, init, update, view)

import Html
import Html.Attributes
import Html.Events
import Shared
import Types exposing (DemosModel)
import Url exposing (Url)
import View exposing (View)
import View.Post



-- INIT


init : DemosModel
init =
    { search = "" }



-- UPDATE


type Msg
    = Search String
    | Play Url


update : Msg -> DemosModel -> ( DemosModel, Maybe Shared.Msg )
update msg model =
    case msg of
        Search search ->
            ( { model | search = search }, Nothing )

        Play media ->
            ( model, Just (Shared.Play media) )


view : Shared.Model -> DemosModel -> View Msg
view shared model =
    { title = "demos"
    , body =
        [ shared.posts
            |> List.filterMap
                (\post ->
                    if
                        (post.category == "Demo")
                            && View.Post.isMatch model.search post
                    then
                        Just post

                    else
                        Nothing
                )
            |> View.Post.viewList Play shared
        ]
    , toolbar =
        [ Html.label []
            [ Html.text "Search "
            , Html.input
                [ Html.Attributes.type_ "search"
                , Html.Attributes.value model.search
                , Html.Events.onInput Search
                ]
                []
            ]
        ]
    }
