module Backend exposing (app)

import Env
import Http
import Json.Decode exposing (Decoder)
import Lamdera
import Task
import Types exposing (BackendModel, BackendMsg(..), FrontendMsg(..), ToBackend(..), ToFrontend(..), TokenData)
import Url.Builder


app :
    { init : ( BackendModel, Cmd BackendMsg )
    , update : BackendMsg -> BackendModel -> ( BackendModel, Cmd BackendMsg )
    , updateFromFrontend : Lamdera.SessionId -> Lamdera.ClientId -> ToBackend -> BackendModel -> ( BackendModel, Cmd BackendMsg )
    , subscriptions : BackendModel -> Sub BackendMsg
    }
app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = subscriptions
        }


init : ( BackendModel, Cmd backendMsg )
init =
    ( {}, Cmd.none )


updateFromFrontend :
    Lamdera.SessionId
    -> Lamdera.ClientId
    -> ToBackend
    -> BackendModel
    -> ( BackendModel, Cmd BackendMsg )
updateFromFrontend _ clientId msg model =
    case msg of
        GetTokenRequest { code } ->
            ( model
            , Http.task
                { method = "POST"
                , headers = []
                , timeout = Just 5000
                , url = Url.Builder.crossOrigin "https://www.patreon.com" [ "api", "oauth2", "token" ] []
                , body =
                    Http.multipartBody
                        [ Http.stringPart "code" code
                        , Http.stringPart "grant_type" "authorization_code"
                        , Http.stringPart "client_id" Env.clientId
                        , Http.stringPart "client_secret" Env.clientSecret
                        , Http.stringPart "redirect_uri" "https://uriel.tail1b193.ts.net/"
                        ]
                , resolver =
                    jsonResolver
                        (Json.Decode.map4 TokenData
                            (Json.Decode.field "access_token" Json.Decode.string)
                            (Json.Decode.field "expires_in" Json.Decode.int)
                            (Json.Decode.field "refresh_token" Json.Decode.string)
                            (Json.Decode.field "scope" <| Json.Decode.map (String.split " ") Json.Decode.string)
                        )
                }
                |> Task.attempt (GotToken clientId)
            )

        GetIdentityRequest { accessToken } ->
            ( model
            , Http.task
                { method = "GET"
                , headers = [ Http.header "Authorization" ("Bearer " ++ accessToken) ]
                , url =
                    Url.Builder.crossOrigin "https://www.patreon.com"
                        [ "api", "oauth2", "v2", "identity" ]
                        [ Url.Builder.string "include" "memberships.currently_entitled_tiers.campaign"
                        , Url.Builder.string "fields[tier]" "title"
                        ]
                , body = Http.emptyBody
                , resolver = jsonResolver identityDecoder
                , timeout = Nothing
                }
                |> Task.attempt (GotIdentity clientId)
            )


identityDecoder : Decoder String
identityDecoder =
    Json.Decode.field "included"
        (Json.Decode.list
            (Json.Decode.oneOf
                [ Json.Decode.map2
                    (\campaignId tierTitle ->
                        if campaignId == Env.orlaCampaignId then
                            Just (String.trim tierTitle)

                        else
                            Nothing
                    )
                    (Json.Decode.at [ "relationships", "campaign", "data", "id" ] Json.Decode.string)
                    (Json.Decode.at [ "attributes", "title" ] Json.Decode.string)
                , Json.Decode.succeed Nothing
                ]
            )
        )
        |> Json.Decode.map
            (\list ->
                case List.filterMap identity list of
                    [ "Bronze membership" ] ->
                        "Bronze tier"

                    [ "Silver membership" ] ->
                        "Silver tier"

                    [ "Gold membership" ] ->
                        "Gold tier"

                    [ tier ] ->
                        "Unknown tier: " ++ tier

                    [] ->
                        "You're not a Patron. (Yet!)"

                    res ->
                        "Ambiguous result: " ++ String.join ", " res
            )


jsonResolver : Json.Decode.Decoder a -> Http.Resolver Http.Error a
jsonResolver decoder =
    Http.stringResolver
        (\stringResolverUnpack ->
            case stringResolverUnpack of
                Http.BadUrl_ stringString ->
                    Result.Err (Http.BadUrl stringString)

                Http.Timeout_ ->
                    Result.Err Http.Timeout

                Http.NetworkError_ ->
                    Result.Err Http.NetworkError

                Http.BadStatus_ httpMetadata _ ->
                    Result.Err (Http.BadStatus httpMetadata.statusCode)

                Http.GoodStatus_ _ body ->
                    case Json.Decode.decodeString decoder body of
                        Ok value ->
                            Ok value

                        Result.Err _ ->
                            Result.Err (Http.BadBody body)
        )


update : BackendMsg -> BackendModel -> ( BackendModel, Cmd BackendMsg )
update msg model =
    case msg of
        GotToken clientId (Err e) ->
            let
                _ =
                    Debug.log "GotToken error" e
            in
            ( model, Lamdera.sendToFrontend clientId (GetTokenResponse (Err ())) )

        GotToken clientId (Ok data) ->
            ( model, Lamdera.sendToFrontend clientId (GetTokenResponse (Ok data)) )

        GotIdentity clientId (Err e) ->
            let
                _ =
                    Debug.log "GotIdentity error" e
            in
            ( model, Lamdera.sendToFrontend clientId (GetIdentityResponse (Err ())) )

        GotIdentity clientId (Ok data) ->
            let
                _ =
                    Debug.log "GotIdentity ok" data
            in
            ( model, Lamdera.sendToFrontend clientId (GetIdentityResponse (Ok data)) )


subscriptions : BackendModel -> Sub BackendMsg
subscriptions _ =
    Sub.none
