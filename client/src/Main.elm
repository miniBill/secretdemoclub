port module Main exposing (Flags, Model, Msg, main)

import AppUrl
import Browser
import Browser.Navigation exposing (Key)
import Cmd.Extra
import Dict
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events
import Http
import Json.Decode
import Json.Encode
import Parser exposing ((|.), (|=), Parser)
import Parser.Workaround
import Post exposing (Post)
import RemoteData exposing (RemoteData)
import Result.Extra
import Route exposing (Filter, Route(..))
import Route.Error
import Route.Index
import Route.Loading
import Route.Login
import Route.Posts
import Task exposing (Task)
import Time
import Url exposing (Url)
import View exposing (View)


type alias Model =
    { key : Key
    , root : Url
    , filter : Filter
    , indexHash : Maybe String
    , posts : RemoteData Http.Error (List Post)
    , time : Maybe ( Time.Zone, Time.Posix )
    , playing : Maybe String
    , hasServiceWorker : Bool
    }


type Msg
    = LoadedPosts (Result Http.Error { indexHash : String, posts : List Post })
    | HereAndNow Time.Zone Time.Posix
    | Search String
    | Play String
    | OnUrlChange Url
    | OnUrlRequest Browser.UrlRequest
    | ServiceWorkerRegistrationSuccess


type alias Flags =
    Json.Decode.Value


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = OnUrlChange
        , onUrlRequest = OnUrlRequest
        }


init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    let
        flagsDecoder : Json.Decode.Decoder String
        flagsDecoder =
            Json.Decode.field "index" Json.Decode.string

        decodedFlags : Maybe String
        decodedFlags =
            Json.Decode.decodeValue flagsDecoder flags
                |> Result.toMaybe

        route : Route
        route =
            Route.parse url

        ( posts, loadCmd ) =
            case decodedFlags of
                Nothing ->
                    case
                        AppUrl.fromUrl url
                            |> .queryParameters
                            |> Dict.get "code"
                    of
                        Just [ code ] ->
                            ( RemoteData.Loading
                            , loadPostsFromCode code
                                |> Task.attempt LoadedPosts
                            )

                        _ ->
                            RemoteData.NotAsked
                                |> Cmd.Extra.pure

                Just indexUrl ->
                    ( RemoteData.Loading
                    , loadPostsFromIndexHash indexUrl
                        |> Task.attempt LoadedPosts
                    )

        initialModel : Model
        initialModel =
            { key = key
            , root = { url | path = "", query = Nothing, fragment = Nothing }
            , filter = Route.emptyFilter
            , indexHash = decodedFlags
            , hasServiceWorker = False
            , posts = posts
            , time = Nothing
            , playing = Nothing
            }

        hereAndNow : Cmd Msg
        hereAndNow =
            Task.map2 HereAndNow Time.here Time.now
                |> Task.perform identity
    in
    (case route of
        Index filter ->
            ( { initialModel
                | filter = filter
              }
            , loadCmd
            )

        Logout ->
            initialModel
                |> logout
    )
        |> Cmd.Extra.add hereAndNow
        |> Cmd.Extra.andThen replaceUrlWithCurrentRoute


loadPostsFromCode : String -> Task Http.Error { indexHash : String, posts : List Post }
loadPostsFromCode code =
    Http.task
        { method = "POST"
        , body = Http.stringBody "text/plain" code
        , url = "/api"
        , headers = []
        , resolver = Http.stringResolver generalResolver
        , timeout = Nothing
        }
        |> Task.andThen loadPostsFromIndexHash


generalResolver : Http.Response a -> Result Http.Error a
generalResolver response =
    case response of
        Http.BadUrl_ url ->
            Http.BadUrl url |> Err

        Http.Timeout_ ->
            Err Http.Timeout

        Http.NetworkError_ ->
            Err Http.NetworkError

        Http.BadStatus_ metadata _ ->
            Http.BadStatus metadata.statusCode |> Err

        Http.GoodStatus_ _ body ->
            Ok body


loadPostsFromIndexHash : String -> Task Http.Error { indexHash : String, posts : List Post }
loadPostsFromIndexHash indexHash =
    Http.task
        { method = "GET"
        , body = Http.emptyBody
        , url = "/media/" ++ indexHash
        , headers = []
        , resolver = Http.stringResolver generalResolver
        , timeout = Nothing
        }
        |> Task.andThen
            (\list ->
                list
                    |> String.split "\n\n"
                    |> Result.Extra.combineMap
                        (\post ->
                            Parser.run postParser post
                                |> Result.mapError
                                    (\_ ->
                                        Http.BadBody "Could not read posts from the server"
                                    )
                        )
                    |> resultToTask
            )
        |> Task.map
            (\posts ->
                { indexHash = indexHash
                , posts = posts
                }
            )


resultToTask : Result error a -> Task error a
resultToTask result =
    case result of
        Ok o ->
            Task.succeed o

        Err e ->
            Task.fail e


postParser : Parser Post
postParser =
    let
        line : String -> (String -> Maybe a) -> Parser a
        line label validate =
            Parser.succeed identity
                |. Parser.symbol label
                |. Parser.spaces
                |. Parser.symbol ":"
                |= (Parser.Workaround.chompUntilEndOrAfter "\n" |> Parser.getChompedString)
                |> Parser.andThen
                    (\raw ->
                        case validate (String.trim raw) of
                            Just res ->
                                Parser.succeed res

                            Nothing ->
                                Parser.problem ("Wrong input: " ++ raw)
                    )
    in
    Parser.succeed Post
        |= line "Title" Just
        |= line "Category" Just
        |= line "Date"
            (\date ->
                date
                    |> String.toInt
                    |> Maybe.map Time.millisToPosix
            )
        |= line "Image" Just
        |= line "Link" Url.fromString
        |= line "Media" Just
        |= Parser.oneOf
            [ Parser.succeed Just
                |= line "Number" Just
            , Parser.succeed Nothing
            ]
        |. Parser.end


view : Model -> Browser.Document Msg
view model =
    let
        content : View Msg
        content =
            case model.posts of
                RemoteData.Failure err ->
                    Route.Error.view err

                RemoteData.Loading ->
                    Route.Loading.view

                RemoteData.NotAsked ->
                    if model.filter == Route.emptyFilter then
                        Route.Index.view model

                    else
                        Route.Login.view model

                RemoteData.Success posts ->
                    innerView model posts
    in
    { title =
        case content.title of
            Nothing ->
                "Secret Demo Club HQ"

            Just title ->
                "Secret Demo Club HQ - " ++ title
    , body =
        [ [ header model
          , Html.div [ HA.style "padding" "0 8px" ] content.body
          , playerView model
          ]
            |> Html.div
                [ HA.style "gap" "8px"
                , HA.style "display" "flex"
                , HA.style "flex-direction" "column"
                ]
        ]
    }


header : Model -> Html Msg
header model =
    Html.div
        [ HA.style "display" "flex"
        , HA.style "position" "sticky"
        , HA.style "top" "0"
        , HA.style "left" "0"
        , HA.style "right" "0"
        , HA.style "z-index" "1"
        , HA.style "background-color" "var(--red)"
        , HA.style "padding" "8px"
        , HA.style "align-items" "baseline"
        ]
        [ Html.div [ HA.style "flex" "1" ]
            [ Html.a [ HA.href "/" ] [ Html.text "Secret Demo Club HQ" ]
            ]
        , Html.div
            [ HA.style "display" "flex"
            , HA.style "align-items" "baseline"
            , HA.style "gap" "8px"
            ]
            [ Html.label []
                [ Html.text "Search "
                , Html.input
                    [ HA.type_ "search"
                    , HA.value model.filter.search
                    , Html.Events.onInput Search
                    ]
                    []
                ]
            , Html.text " "
            , case model.indexHash of
                Just _ ->
                    Html.a
                        [ HA.href "/logout" ]
                        [ Html.text "Logout" ]

                Nothing ->
                    Route.Login.loginButton model.root
            ]
        ]


playerView : Model -> Html Msg
playerView model =
    case model.playing of
        Just url ->
            Html.audio
                [ HA.style "position" "sticky"
                , HA.style "bottom" "0"
                , HA.style "left" "0"
                , HA.style "right" "0"
                , HA.style "z-index" "1"
                , HA.style "background-color" "var(--navy)"
                , HA.style "padding" "8px"

                --
                , HA.controls True
                , HA.autoplay True
                , HA.src ("/media/" ++ url)
                ]
                []

        Nothing ->
            Html.text ""


innerView : Model -> List Post -> View Msg
innerView model posts =
    if model.filter == Route.emptyFilter then
        Route.Index.view model

    else
        Route.Posts.view { play = Play } model posts


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Play url ->
            { model | playing = Just url }
                |> Cmd.Extra.pure

        LoadedPosts (Ok { indexHash, posts }) ->
            ( { model
                | indexHash = Just indexHash
                , posts = RemoteData.Success posts
              }
            , saveIndexHash (Just indexHash)
            )

        LoadedPosts (Err err) ->
            { model | posts = RemoteData.Failure err }
                |> Cmd.Extra.pure

        HereAndNow here now ->
            { model | time = Just ( here, now ) }
                |> Cmd.Extra.pure

        Search search ->
            let
                filter : Filter
                filter =
                    model.filter
            in
            { model | filter = { filter | search = search } }
                |> Cmd.Extra.pure
                |> Cmd.Extra.andThen replaceUrlWithCurrentRoute

        OnUrlChange url ->
            let
                route : Route
                route =
                    Route.parse url
            in
            case route of
                Logout ->
                    model
                        |> logout
                        |> Cmd.Extra.andThen replaceUrlWithCurrentRoute

                Index newFilter ->
                    let
                        newModel : Model
                        newModel =
                            { model | filter = newFilter }
                    in
                    if Route.toString route == Route.toString (Index model.filter) then
                        newModel
                            |> Cmd.Extra.pure

                    else
                        newModel
                            |> Cmd.Extra.pure
                            |> Cmd.Extra.andThen replaceUrlWithCurrentRoute

        OnUrlRequest (Browser.Internal url) ->
            ( model, Browser.Navigation.pushUrl model.key (Url.toString url) )

        OnUrlRequest (Browser.External url) ->
            ( model, Browser.Navigation.load url )

        ServiceWorkerRegistrationSuccess ->
            { model | hasServiceWorker = True }
                |> Cmd.Extra.pure


logout : Model -> ( Model, Cmd msg )
logout model =
    ( { model
        | indexHash = Nothing
        , posts = RemoteData.NotAsked
      }
    , saveIndexHash Nothing
    )


replaceUrlWithCurrentRoute : Model -> ( Model, Cmd Msg )
replaceUrlWithCurrentRoute model =
    ( model
    , Browser.Navigation.replaceUrl model.key (Route.toString (Index model.filter))
    )


saveIndexHash : Maybe String -> Cmd msg
saveIndexHash url =
    { key = "index"
    , value =
        case url of
            Just u ->
                Json.Encode.string u

            Nothing ->
                Json.Encode.null
    }
        |> sendToLocalStorage


port sendToLocalStorage :
    { key : String
    , value : Json.Encode.Value
    }
    -> Cmd msg


port serviceWorkerRegistrationSuccess : ({} -> msg) -> Sub msg


subscriptions : model -> Sub Msg
subscriptions _ =
    serviceWorkerRegistrationSuccess (\_ -> ServiceWorkerRegistrationSuccess)
