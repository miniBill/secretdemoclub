port module Main exposing (Flags, Model, Msg, main)

import AppUrl
import Browser
import Browser.Navigation exposing (Key)
import ConcurrentTask exposing (ConcurrentTask)
import ConcurrentTask.Http as Http
import Dict
import Html
import Html.Attributes
import Html.Events
import Json.Decode
import Json.Encode
import List.Extra
import Post exposing (Post)
import RemoteData exposing (RemoteData)
import Route exposing (Route(..))
import Route.Error
import Route.Index
import Route.Loading
import Route.Login
import Route.Posts
import Task
import Time
import Url exposing (Url)
import View exposing (View)


type alias Model =
    { key : Key
    , root : Url
    , search : String
    , route : Route
    , pool : ConcurrentTask.Pool Msg Http.Error { index : String, posts : List Post }
    , index : Maybe String
    , posts : RemoteData Http.Error (List Post)
    , time : Maybe ( Time.Zone, Time.Posix )
    , playing : Maybe Url
    }


type Msg
    = LoadedPosts (ConcurrentTask.Response Http.Error { index : String, posts : List Post })
    | HereAndNow Time.Zone Time.Posix
    | Search String
    | Play Url
    | OnUrlChange Url
    | OnUrlRequest Browser.UrlRequest
    | OnProgress ( ConcurrentTask.Pool Msg Http.Error { index : String, posts : List Post }, Cmd Msg )


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

        decodedFlags : Result String String
        decodedFlags =
            Json.Decode.decodeValue flagsDecoder flags
                |> Result.mapError Json.Decode.errorToString

        { search, route } =
            Route.parse url

        _ =
            case decodedFlags of
                Ok _ ->
                    ()

                Err e ->
                    let
                        _ =
                            Debug.log "Error decoding flags" e
                    in
                    ()

        index : Maybe String
        index =
            Result.toMaybe decodedFlags

        ( pool, loadCmd ) =
            case index of
                Nothing ->
                    case
                        AppUrl.fromUrl url
                            |> .queryParameters
                            |> Dict.get "code"
                    of
                        Just [ code ] ->
                            loadPostsFromCode code
                                |> ConcurrentTask.attempt
                                    { pool = ConcurrentTask.pool
                                    , send = send
                                    , onComplete = LoadedPosts
                                    }

                        _ ->
                            ( ConcurrentTask.pool, Cmd.none )

                Just indexUrl ->
                    loadPostsFromIndex indexUrl
                        |> ConcurrentTask.attempt
                            { pool = ConcurrentTask.pool
                            , send = send
                            , onComplete = LoadedPosts
                            }

        model : Model
        model =
            { key = key
            , root = { url | path = "", query = Nothing, fragment = Nothing }
            , route = route
            , search = search
            , pool = pool
            , index = index
            , posts = RemoteData.NotAsked
            , time = Nothing
            , playing = Nothing
            }
    in
    ( model
    , Cmd.batch
        [ loadCmd
        , Task.map2 HereAndNow Time.here Time.now
            |> Task.perform identity
        ]
    )


loadPostsFromCode : String -> ConcurrentTask Http.Error { index : String, posts : List Post }
loadPostsFromCode code =
    Http.post
        { body = Http.jsonBody (Json.Encode.string code)
        , url = "/api"
        , headers = []
        , expect = Http.expectJson Json.Decode.string
        , timeout = Nothing
        }
        |> ConcurrentTask.andThen loadPostsFromIndex


loadPostsFromIndex : String -> ConcurrentTask Http.Error { index : String, posts : List Post }
loadPostsFromIndex url =
    Http.get
        { url = "/media/" ++ url
        , headers = []
        , expect = Http.expectString
        , timeout = Nothing
        }
        |> ConcurrentTask.andThen
            (\list ->
                list
                    |> String.split ""
                    |> List.Extra.removeWhen String.isEmpty
                    |> Debug.todo "TODO"
            )


urlDecoder : Json.Decode.Decoder Url
urlDecoder =
    Json.Decode.string
        |> Json.Decode.andThen
            (\indexString ->
                case Url.fromString indexString of
                    Nothing ->
                        Json.Decode.fail ("Invalid URL: " ++ indexString)

                    Just url ->
                        Json.Decode.succeed url
            )


view : Model -> Browser.Document Msg
view model =
    let
        content : View Msg
        content =
            if model.route == Index && String.isEmpty model.search then
                Route.Index.view model

            else
                case model.posts of
                    RemoteData.NotAsked ->
                        Route.Login.view model

                    RemoteData.Failure err ->
                        Route.Error.view err

                    RemoteData.Loading ->
                        Route.Loading.view

                    RemoteData.Success posts ->
                        Route.Posts.view { play = Play } model posts
    in
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
                        [ Html.Attributes.href "/" ]
                        [ Html.text "Secret Demo Club HQ" ]
                    ]
                , Html.div []
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
                ]
          , Html.div [ Html.Attributes.style "padding" "0 8px" ] content.body
          , case model.playing of
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
                        , Html.Attributes.src (Url.toString url)
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
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Play url ->
            ( { model | playing = Just url }, Cmd.none )

        LoadedPosts (ConcurrentTask.Success { index, posts }) ->
            ( { model | index = Just index, posts = RemoteData.Success posts }, saveIndex index )

        LoadedPosts (ConcurrentTask.Error err) ->
            ( { model | posts = RemoteData.Failure err }, Cmd.none )

        LoadedPosts (ConcurrentTask.UnexpectedError err) ->
            let
                _ =
                    Debug.log "error" err
            in
            ( { model
                | posts =
                    RemoteData.Failure Http.NetworkError
              }
            , Cmd.none
            )

        HereAndNow here now ->
            ( { model | time = Just ( here, now ) }, Cmd.none )

        Search search ->
            changeRouteTo { model | search = search }

        OnUrlChange url ->
            let
                { route, search } =
                    Route.parse url
            in
            changeRouteTo { model | route = route, search = search }

        OnUrlRequest (Browser.Internal url) ->
            ( model, Browser.Navigation.pushUrl model.key (Url.toString url) )

        OnUrlRequest (Browser.External url) ->
            ( model, Browser.Navigation.load url )

        OnProgress ( pool, cmd ) ->
            ( { model | pool = pool }, cmd )


changeRouteTo : Model -> ( Model, Cmd Msg )
changeRouteTo model =
    ( model, Browser.Navigation.replaceUrl model.key (Route.toString model) )


saveIndex : String -> Cmd msg
saveIndex url =
    { key = "index"
    , value = url
    }
        |> sendToLocalStorage


port sendToLocalStorage :
    { key : String
    , value : String
    }
    -> Cmd msg


subscriptions : Model -> Sub Msg
subscriptions model =
    ConcurrentTask.onProgress
        { send = send
        , receive = receive
        , onProgress = OnProgress
        }
        model.pool


port send : Json.Decode.Value -> Cmd msg


port receive : (Json.Decode.Value -> msg) -> Sub msg
