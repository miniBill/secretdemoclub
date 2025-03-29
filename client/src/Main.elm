port module Main exposing (Flags, Model, Msg, main)

import AppUrl
import Browser
import Browser.Navigation exposing (Key)
import Dict
import Html
import Html.Attributes
import Html.Events
import Http
import Json.Decode
import List.Extra
import Parser exposing ((|.), (|=), Parser)
import Parser.Workaround
import Post exposing (Post)
import RemoteData exposing (RemoteData)
import Route exposing (Route(..))
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
    , search : String
    , route : Route
    , index : Maybe String
    , posts : RemoteData Http.Error (List Post)
    , time : Maybe ( Time.Zone, Time.Posix )
    , playing : Maybe String
    }


type Msg
    = LoadedPosts (Result Http.Error { index : String, posts : List Post })
    | HereAndNow Time.Zone Time.Posix
    | Search String
    | Play String
    | OnUrlChange Url
    | OnUrlRequest Browser.UrlRequest


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

        index : Maybe String
        index =
            Result.toMaybe decodedFlags

        loadCmd : Cmd Msg
        loadCmd =
            case index of
                Nothing ->
                    case
                        AppUrl.fromUrl url
                            |> .queryParameters
                            |> Dict.get "code"
                    of
                        Just [ code ] ->
                            loadPostsFromCode code
                                |> Task.attempt LoadedPosts

                        _ ->
                            Cmd.none

                Just indexUrl ->
                    loadPostsFromIndex indexUrl
                        |> Task.attempt LoadedPosts

        model : Model
        model =
            { key = key
            , root = { url | path = "", query = Nothing, fragment = Nothing }
            , route = route
            , search = search
            , index = index
            , posts = RemoteData.NotAsked
            , time = Nothing
            , playing = Nothing
            }
    in
    ( model
    , Cmd.batch
        [ loadCmd
        , Browser.Navigation.replaceUrl model.key (Route.toString model)
        , Task.map2 HereAndNow Time.here Time.now
            |> Task.perform identity
        ]
    )


loadPostsFromCode : String -> Task Http.Error { index : String, posts : List Post }
loadPostsFromCode code =
    Http.task
        { method = "POST"
        , body = Http.stringBody "text/plain" code
        , url = "/api"
        , headers = []
        , resolver = Http.stringResolver generalResolver
        , timeout = Nothing
        }
        |> Task.andThen loadPostsFromIndex


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


loadPostsFromIndex : String -> Task Http.Error { index : String, posts : List Post }
loadPostsFromIndex index =
    Http.task
        { method = "GET"
        , body = Http.emptyBody
        , url = "/media/" ++ index
        , headers = []
        , resolver = Http.stringResolver generalResolver
        , timeout = Nothing
        }
        |> Task.andThen
            (\list ->
                list
                    |> String.split "\n"
                    |> List.Extra.removeWhen String.isEmpty
                    |> List.map
                        (\postUrl ->
                            Http.task
                                { method = "GET"
                                , body = Http.emptyBody
                                , url = "/media/" ++ postUrl
                                , headers = []
                                , resolver = Http.stringResolver generalResolver
                                , timeout = Nothing
                                }
                                |> Task.andThen
                                    (\body ->
                                        case Parser.run postParser body of
                                            Ok post ->
                                                Task.succeed post

                                            Err e ->
                                                let
                                                    _ =
                                                        Debug.log "error parsing post" e
                                                in
                                                Task.fail (Http.BadBody (Debug.toString e))
                                    )
                        )
                    |> Task.sequence
            )
        |> Task.map
            (\posts ->
                { index = index
                , posts = posts
                }
            )


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
                    if model.route == Index && String.isEmpty model.search then
                        Route.Index.view model

                    else
                        Route.Login.view model

                RemoteData.Success posts ->
                    if model.route == Index && String.isEmpty model.search then
                        Route.Index.view model

                    else
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
                        , Html.Attributes.src ("/media/" ++ url)
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

        LoadedPosts (Ok { index, posts }) ->
            ( { model | index = Just index, posts = RemoteData.Success posts }, saveIndex index )

        LoadedPosts (Err err) ->
            ( { model | posts = RemoteData.Failure err }, Cmd.none )

        HereAndNow here now ->
            ( { model | time = Just ( here, now ) }, Cmd.none )

        Search search ->
            changeRouteTo { model | search = search }

        OnUrlChange url ->
            let
                { route, search } =
                    Route.parse url

                newModel : Model
                newModel =
                    { model | route = route, search = search }
            in
            if Route.toString model == Route.toString newModel then
                ( newModel, Cmd.none )

            else
                changeRouteTo newModel

        OnUrlRequest (Browser.Internal url) ->
            ( model, Browser.Navigation.pushUrl model.key (Url.toString url) )

        OnUrlRequest (Browser.External url) ->
            ( model, Browser.Navigation.load url )


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


subscriptions : model -> Sub msg
subscriptions _ =
    Sub.none
