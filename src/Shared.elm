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
import Rss exposing (Rss)
import Serialize
import Shared.Model
import Shared.Msg



-- FLAGS


type alias Flags =
    { rss : Maybe { url : String, rss : Rss } }


decoder : Json.Decode.Decoder Flags
decoder =
    Json.Decode.map Flags
        (Json.Decode.field
            "rss"
            (Json.Decode.maybe
                (Json.Decode.map2 (\url rss -> { url = url, rss = rss })
                    (Json.Decode.field "url" Json.Decode.string)
                    (Json.Decode.field "rss" Json.Decode.string
                        |> Json.Decode.andThen
                            (\raw ->
                                case Base64.toBytes raw of
                                    Just bytes ->
                                        case Serialize.decodeFromBytes Rss.lastCodec bytes of
                                            Ok rss ->
                                                Json.Decode.succeed rss

                                            Err _ ->
                                                Json.Decode.fail "Invalid bytes encoding"

                                    Nothing ->
                                        Json.Decode.fail "Invalid base64 encoding"
                            )
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
        flags : Flags
        flags =
            flagsResult
                |> Result.withDefault { rss = Nothing }
    in
    ( { rss = flags.rss }
    , Effect.none
    )



-- UPDATE


type alias Msg =
    Shared.Msg.Msg


update : Route () -> Msg -> Model -> ( Model, Effect Msg )
update route msg model =
    case msg of
        Shared.Msg.LoadedRss data ->
            ( { model | rss = Just data }
            , Effect.batch
                [ route.query
                    |> Dict.get "from"
                    |> Maybe.andThen Path.fromString
                    |> Maybe.withDefault Path.Home_
                    |> Effect.pushRoutePath
                , Effect.saveRss data
                ]
            )



-- SUBSCRIPTIONS


subscriptions : Route () -> Model -> Sub Msg
subscriptions _ _ =
    Sub.none
