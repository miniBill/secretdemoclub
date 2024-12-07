module Pages.LoadRss exposing (Model, Msg, page)

import Effect exposing (Effect)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Http
import Page exposing (Page)
import Route exposing (Route)
import Rss exposing (Post)
import Rss.Parser
import Shared
import Url
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page shared _ =
    Page.new
        { init = init shared
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- INIT


type alias Model =
    { rssUrl : String
    , loading : Bool
    , error : Maybe String
    }


init : Shared.Model -> () -> ( Model, Effect Msg )
init shared _ =
    ( { rssUrl = shared.rss.url
      , loading = False
      , error = Nothing
      }
    , Effect.none
    )



-- UPDATE


type Msg
    = UserUpdatedInput String
    | UserPressedLoad
    | FetchRssResult (Result String { url : String, posts : List Post })


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        UserUpdatedInput rssUrl ->
            ( { model
                | rssUrl = rssUrl
              }
            , Effect.none
            )

        UserPressedLoad ->
            case Url.fromString model.rssUrl of
                Just url ->
                    ( { model
                        | loading = True
                        , error = Nothing
                      }
                    , Effect.sendCmd
                        (Http.get
                            { url = url.path ++ "?" ++ Maybe.withDefault "" url.query
                            , expect =
                                Http.expectString
                                    (\result ->
                                        result
                                            |> Result.mapError (\_ -> "Failed to get the RSS feed. Check your connection.")
                                            |> Result.andThen
                                                (\raw ->
                                                    raw
                                                        |> Rss.Parser.parse
                                                        |> Result.mapError
                                                            (\{ error, path } ->
                                                                let
                                                                    _ =
                                                                        Debug.log "path" path
                                                                in
                                                                Rss.Parser.errorToString error
                                                            )
                                                )
                                            |> Result.map (\posts -> { url = model.rssUrl, posts = posts })
                                            |> FetchRssResult
                                    )
                            }
                        )
                    )

                Nothing ->
                    ( { model | error = Just "Invalid URL" }, Effect.none )

        FetchRssResult (Err e) ->
            let
                _ =
                    Debug.log "Error getting RSS" e
            in
            ( { model
                | error = Just "Failed getting RSS"
              }
            , Effect.none
            )

        FetchRssResult (Ok data) ->
            ( { model | loading = False }, Effect.loadedRss data )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Load RSS feed"
    , body = [ viewPage model ]
    }


viewPage : Model -> Html Msg
viewPage model =
    if model.loading then
        Html.text ("Loading " ++ model.rssUrl ++ "...")

    else
        Html.div
            [ Html.Attributes.style "display" "flex"
            , Html.Attributes.style "width" "100%"
            , Html.Attributes.style "gap" "8px"
            , Html.Attributes.style "flex-direction" "column"
            ]
            [ Html.div
                [ Html.Attributes.style "display" "flex"
                , Html.Attributes.style "width" "100%"
                , Html.Attributes.style "gap" "8px"
                ]
                [ Html.label
                    [ Html.Attributes.style "display" "flex"
                    , Html.Attributes.style "flex-grow" "1"
                    , Html.Attributes.style "gap" "8px"
                    ]
                    [ Html.span []
                        [ Html.text "Insert the "
                        , Html.a
                            [ Html.Attributes.href "https://www.patreon.com/c/orlagartland/membership"
                            , Html.Attributes.target "_blank"
                            ]
                            [ Html.text "podcast URL" ]
                        ]
                    , Html.input
                        [ Html.Attributes.type_ "url"
                        , Html.Attributes.value model.rssUrl
                        , Html.Events.onInput UserUpdatedInput
                        , Html.Attributes.style "flex-grow" "1"
                        ]
                        []
                    ]
                , Html.text " "
                , Html.button
                    [ Html.Events.onClick UserPressedLoad ]
                    [ Html.text "Load" ]
                ]
            , case model.error of
                Nothing ->
                    Html.text ""

                Just error ->
                    Html.div
                        [ Html.Attributes.style "background" "#ffcccc" ]
                        [ Html.text error ]
            ]
