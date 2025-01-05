module Frontend exposing (app)

import AppUrl exposing (AppUrl)
import Browser
import Browser.Navigation
import Dict
import Env
import Html exposing (Html)
import Html.Attributes
import Lamdera exposing (Key, Url)
import Types exposing (FrontendModel, FrontendMsg(..), InnerModel(..), ToBackend(..), ToFrontend(..))
import Url
import Url.Builder


app :
    { init : Url -> Browser.Navigation.Key -> ( FrontendModel, Cmd FrontendMsg )
    , view : FrontendModel -> Browser.Document FrontendMsg
    , update : FrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
    , updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
    , subscriptions : FrontendModel -> Sub FrontendMsg
    , onUrlRequest : Browser.UrlRequest -> FrontendMsg
    , onUrlChange : Url -> FrontendMsg
    }
app =
    Lamdera.frontend
        { init = init
        , update = update
        , view = view
        , onUrlChange = OnUrlChange
        , onUrlRequest = OnUrlRequest
        , updateFromBackend = updateFromBackend
        , subscriptions = subscriptions
        }


init : Url -> Key -> ( FrontendModel, Cmd FrontendMsg )
init url key =
    let
        appUrl : AppUrl
        appUrl =
            AppUrl.fromUrl url
    in
    case Dict.get "code" appUrl.queryParameters of
        Just [ code ] ->
            ( { key = key
              , inner = WithCode code
              }
            , Cmd.batch
                [ Lamdera.sendToBackend (GetTierRequest { code = code })
                , Browser.Navigation.replaceUrl key "/"
                ]
            )

        _ ->
            ( { key = key
              , inner = Initial
              }
            , Cmd.none
            )


view : FrontendModel -> Browser.Document FrontendMsg
view model =
    { title = "TODO"
    , body =
        Html.node "style"
            []
            [ Html.text
                """
                body {
                    padding: 8px;
                }"""
            ]
            :: (case model.inner of
                    Initial ->
                        [ loginLink ]

                    WithCode code ->
                        [ Html.text ("Code: " ++ code) ]

                    CouldNotGetTier ->
                        [ Html.text "Could not get tier."
                        , Html.br [] []
                        , loginLink
                        ]

                    WithTier tier ->
                        [ Html.text ("You are: " ++ tier)
                        ]
               )
    }


loginLink : Html frontendMsg
loginLink =
    Html.a
        [ Html.Attributes.href
            (Url.Builder.crossOrigin "http://www.patreon.com"
                [ "oauth2", "authorize" ]
                [ Url.Builder.string "response_type" "code"
                , Url.Builder.string "client_id" Env.clientId
                , Url.Builder.string "redirect_uri" "https://uriel.tail1b193.ts.net/"

                -- , Url.Builder.string "scope" "identity identity.memberships"
                , Url.Builder.string "scope" "identity.memberships"

                -- , Url.Builder.string "scope" "identity"
                ]
            )
        ]
        [ Html.text "Login" ]


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        GetTierResponse (Err ()) ->
            ( { model | inner = CouldNotGetTier }, Cmd.none )

        GetTierResponse (Ok identity) ->
            ( { model | inner = WithTier identity }, Cmd.none )


update : FrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
update msg model =
    case msg of
        OnUrlRequest (Browser.Internal url) ->
            ( model, Browser.Navigation.pushUrl model.key (Url.toString url) )

        OnUrlRequest (Browser.External url) ->
            ( model, Browser.Navigation.load url )

        OnUrlChange _ ->
            ( model, Cmd.none )


subscriptions : FrontendModel -> Sub FrontendMsg
subscriptions _ =
    Sub.none
