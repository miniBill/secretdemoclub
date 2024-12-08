module Layouts.Default exposing (Model, Msg, Props, layout)

import Dict
import Effect exposing (Effect)
import Html
import Html.Attributes
import Layout exposing (Layout)
import Route exposing (Route)
import Route.Path
import Shared
import View exposing (View)


type alias Props =
    {}


layout : Props -> Shared.Model -> Route () -> Layout () Model Msg contentMsg
layout _ shared _ =
    Layout.new
        { init = init
        , update = update
        , view = view shared
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    {}


init : () -> ( Model, Effect Msg )
init _ =
    ( {}, Effect.none )



-- UPDATE


type Msg
    = Noop


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Effect.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view :
    Shared.Model
    ->
        { toContentMsg : Msg -> contentMsg
        , content : View contentMsg
        , model : Model
        }
    -> View contentMsg
view shared { content } =
    { title =
        if String.isEmpty content.title then
            "Secret Demo Club HQ"

        else
            "Secret Demo Club HQ - " ++ content.title
    , body =
        [ [ Html.div
                [ Html.Attributes.style "display" "flex"
                , Html.Attributes.style "position" "sticky"
                , Html.Attributes.style "top" "0"
                , Html.Attributes.style "left" "0"
                , Html.Attributes.style "right" "0"
                , Html.Attributes.style "z-index" "1"
                , Html.Attributes.style "background-color" "black"
                , Html.Attributes.style "padding" "8px"
                ]
                [ Html.div [ Html.Attributes.style "flex" "1" ]
                    [ Html.a
                        [ Route.href
                            { path = Route.Path.Home_
                            , hash = Nothing
                            , query = Dict.empty
                            }
                        ]
                        [ Html.text "Secret Demo Club HQ" ]
                    ]
                , Html.div [] content.toolbar
                ]
          , Html.div [ Html.Attributes.style "padding" "0 8px" ] content.body
          , case shared.playing of
                Just url ->
                    Html.audio
                        [ Html.Attributes.style "position" "sticky"
                        , Html.Attributes.style "bottom" "0"
                        , Html.Attributes.style "left" "0"
                        , Html.Attributes.style "right" "0"
                        , Html.Attributes.style "z-index" "1"
                        , Html.Attributes.style "background-color" "black"
                        , Html.Attributes.style "padding" "8px"

                        --
                        , Html.Attributes.controls True
                        , Html.Attributes.autoplay True
                        , Html.Attributes.src url
                        ]
                        []

                Nothing ->
                    Html.text ""
          ]
            |> Html.div
                [ Html.Attributes.style "gap" "8px"
                , Html.Attributes.style "display" "flex"
                , Html.Attributes.style "flex-direction" "column"
                ]
        ]
    , toolbar = []
    }
