module Shared exposing
    ( Flags, decoder
    , Model, Msg
    , init, update, subscriptions
    )

{-|

@docs Flags, decoder
@docs Model, Msg
@docs init, update, subscriptions

-}

import Base64
import Dict
import Effect exposing (Effect)
import Json.Decode
import Route exposing (Route)
import Route.Path as Path
import Rss exposing (Post)
import Rss.Parser
import Serialize
import Shared.Model
import Shared.Msg



-- FLAGS


type alias Flags =
    { rss : { url : String, posts : List Post } }


decoder : Json.Decode.Decoder Flags
decoder =
    Json.Decode.map Flags
        (Json.Decode.field
            "rss"
            (Json.Decode.map2 (\url posts -> { url = url, posts = posts })
                (Json.Decode.field "url" Json.Decode.string)
                (Json.Decode.field "posts" Json.Decode.string
                    |> Json.Decode.andThen
                        (\raw ->
                            case Base64.toBytes raw of
                                Just bytes ->
                                    case Serialize.decodeFromBytes Rss.lastCodec bytes of
                                        Ok posts ->
                                            Json.Decode.succeed posts

                                        Err _ ->
                                            Json.Decode.fail "Invalid bytes encoding"

                                Nothing ->
                                    Json.Decode.fail "Invalid base64 encoding"
                        )
                )
            )
        )



-- INIT


type alias Model =
    Shared.Model.Model


init : Result Json.Decode.Error Flags -> Route () -> ( Model, Effect Msg )
init flagsResult _ =
    let
        model : Model
        model =
            flagsResult
                |> Result.withDefault
                    { rss =
                        { url = Rss.Parser.rssPrefix
                        , posts = []
                        }
                    }
    in
    ( model, Effect.none )



-- UPDATE


type alias Msg =
    Shared.Msg.Msg


update : Route () -> Msg -> Model -> ( Model, Effect Msg )
update route msg ({ rss } as model) =
    case msg of
        Shared.Msg.LoadedRss newRss ->
            ( { model | rss = newRss }
            , Effect.batch
                [ route.query
                    |> Dict.get "from"
                    |> Maybe.andThen Path.fromString
                    |> Maybe.withDefault Path.Home_
                    |> Effect.pushRoutePath
                , Effect.saveRss newRss
                ]
            )

        Shared.Msg.Logout ->
            ( { model | rss = { rss | posts = [] } }, Effect.pushRoutePath Path.Home_ )



-- SUBSCRIPTIONS


subscriptions : Route () -> Model -> Sub Msg
subscriptions _ _ =
    Sub.none
