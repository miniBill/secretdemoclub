module Layouts.Default exposing (Model, Msg, Props, layout)

import Dict
import Effect exposing (Effect)
import Html
import Html.Attributes exposing (class)
import Html.Events
import Layout exposing (Layout)
import Route exposing (Route)
import Route.Path
import Shared
import View exposing (View)


type alias Props =
    {}


layout : Props -> Shared.Model -> Route () -> Layout () Model Msg contentMsg
layout _ _ _ =
    Layout.new
        { init = init
        , update = update
        , view = view
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
    = Logout


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        Logout ->
            ( model, Effect.logout )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view :
    { toContentMsg : Msg -> contentMsg
    , content : View contentMsg
    , model : Model
    }
    -> View contentMsg
view { toContentMsg, content } =
    { title =
        if String.isEmpty content.title then
            "Secret Demo Club HQ"

        else
            "Secret Demo Club HQ - " ++ content.title
    , body =
        [ Html.div
            [ Html.Attributes.style "display" "flex"
            ]
            [ Html.a
                [ Route.href
                    { path = Route.Path.Home_
                    , hash = Nothing
                    , query = Dict.empty
                    }
                ]
                [ Html.text "Secret Demo Club HQ" ]
            , Html.button
                [ Html.Events.onClick (toContentMsg Logout)
                ]
                [ Html.text "Logout" ]
            ]
        , Html.div [ class "page" ] content.body
        ]
    }
