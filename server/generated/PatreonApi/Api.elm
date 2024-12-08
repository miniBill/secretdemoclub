module PatreonApi.Api exposing
    ( getCampaign, getCampaignMembers, getCampaignMembersTask, getCampaignPosts, getCampaignPostsTask
    , getCampaignTask, getCampaigns, getCampaignsTask, getIdentity, getIdentityTask, getMember, getMemberTask
    , getPost, getPostTask
    , createWebhook, createWebhookTask, editWebhook, editWebhookTask, getWebhooks, getWebhooksTask
    )

{-|


## Resources

@docs getCampaign, getCampaignMembers, getCampaignMembersTask, getCampaignPosts, getCampaignPostsTask
@docs getCampaignTask, getCampaigns, getCampaignsTask, getIdentity, getIdentityTask, getMember, getMemberTask
@docs getPost, getPostTask


## Webhooks

@docs createWebhook, createWebhookTask, editWebhook, editWebhookTask, getWebhooks, getWebhooksTask

-}

import Dict
import Http
import Json.Decode
import Json.Encode
import OpenApi.Common
import PatreonApi.Json
import PatreonApi.Types
import Task
import Url.Builder


{-| Create webhook
-}
createWebhook :
    { authorization : { oauth2 : String }
    , toMsg :
        Result (OpenApi.Common.Error PatreonApi.Types.CreateWebhook_Error String) PatreonApi.Types.WebhooksResponse
        -> msg
    , body : Json.Encode.Value
    , params : { user_Agent : String }
    }
    -> Cmd msg
createWebhook config =
    Http.request
        { url =
            Url.Builder.crossOrigin
                "https://patreon.com"
                [ "api", "oauth", "v2", "webhooks" ]
                []
        , method = "POST"
        , headers =
            [ Http.header
                "authorization"
                ("Bearer " ++ config.authorization.oauth2)
            , Http.header "User-Agent" config.params.user_Agent
            ]
        , expect =
            OpenApi.Common.expectJsonCustom
                config.toMsg
                (Dict.fromList
                    [ ( "400"
                      , Json.Decode.map
                            PatreonApi.Types.CreateWebhook_400
                            PatreonApi.Json.decodeStatusCode400
                      )
                    , ( "401"
                      , Json.Decode.map
                            PatreonApi.Types.CreateWebhook_401
                            PatreonApi.Json.decodeStatusCode401
                      )
                    , ( "403"
                      , Json.Decode.map
                            PatreonApi.Types.CreateWebhook_403
                            PatreonApi.Json.decodeStatusCode403
                      )
                    , ( "404"
                      , Json.Decode.map
                            PatreonApi.Types.CreateWebhook_404
                            PatreonApi.Json.decodeStatusCode404
                      )
                    , ( "405"
                      , Json.Decode.map
                            PatreonApi.Types.CreateWebhook_405
                            PatreonApi.Json.decodeStatusCode405
                      )
                    , ( "406"
                      , Json.Decode.map
                            PatreonApi.Types.CreateWebhook_406
                            PatreonApi.Json.decodeStatusCode406
                      )
                    , ( "410"
                      , Json.Decode.map
                            PatreonApi.Types.CreateWebhook_410
                            PatreonApi.Json.decodeStatusCode410
                      )
                    , ( "429"
                      , Json.Decode.map
                            PatreonApi.Types.CreateWebhook_429
                            PatreonApi.Json.decodeStatusCode429
                      )
                    , ( "500"
                      , Json.Decode.map
                            PatreonApi.Types.CreateWebhook_500
                            PatreonApi.Json.decodeStatusCode500
                      )
                    , ( "503"
                      , Json.Decode.map
                            PatreonApi.Types.CreateWebhook_503
                            PatreonApi.Json.decodeStatusCode503
                      )
                    ]
                )
                PatreonApi.Json.decodeWebhooksResponse
        , body = Http.jsonBody (Basics.identity config.body)
        , timeout = Nothing
        , tracker = Nothing
        }


{-| Create webhook
-}
createWebhookTask :
    { authorization : { oauth2 : String }
    , body : Json.Encode.Value
    , params : { user_Agent : String }
    }
    -> Task.Task (OpenApi.Common.Error PatreonApi.Types.CreateWebhook_Error String) PatreonApi.Types.WebhooksResponse
createWebhookTask config =
    Http.task
        { url =
            Url.Builder.crossOrigin
                "https://patreon.com"
                [ "api", "oauth", "v2", "webhooks" ]
                []
        , method = "POST"
        , headers =
            [ Http.header
                "authorization"
                ("Bearer " ++ config.authorization.oauth2)
            , Http.header "User-Agent" config.params.user_Agent
            ]
        , resolver =
            OpenApi.Common.jsonResolverCustom
                (Dict.fromList
                    [ ( "400"
                      , Json.Decode.map
                            PatreonApi.Types.CreateWebhook_400
                            PatreonApi.Json.decodeStatusCode400
                      )
                    , ( "401"
                      , Json.Decode.map
                            PatreonApi.Types.CreateWebhook_401
                            PatreonApi.Json.decodeStatusCode401
                      )
                    , ( "403"
                      , Json.Decode.map
                            PatreonApi.Types.CreateWebhook_403
                            PatreonApi.Json.decodeStatusCode403
                      )
                    , ( "404"
                      , Json.Decode.map
                            PatreonApi.Types.CreateWebhook_404
                            PatreonApi.Json.decodeStatusCode404
                      )
                    , ( "405"
                      , Json.Decode.map
                            PatreonApi.Types.CreateWebhook_405
                            PatreonApi.Json.decodeStatusCode405
                      )
                    , ( "406"
                      , Json.Decode.map
                            PatreonApi.Types.CreateWebhook_406
                            PatreonApi.Json.decodeStatusCode406
                      )
                    , ( "410"
                      , Json.Decode.map
                            PatreonApi.Types.CreateWebhook_410
                            PatreonApi.Json.decodeStatusCode410
                      )
                    , ( "429"
                      , Json.Decode.map
                            PatreonApi.Types.CreateWebhook_429
                            PatreonApi.Json.decodeStatusCode429
                      )
                    , ( "500"
                      , Json.Decode.map
                            PatreonApi.Types.CreateWebhook_500
                            PatreonApi.Json.decodeStatusCode500
                      )
                    , ( "503"
                      , Json.Decode.map
                            PatreonApi.Types.CreateWebhook_503
                            PatreonApi.Json.decodeStatusCode503
                      )
                    ]
                )
                PatreonApi.Json.decodeWebhooksResponse
        , body = Http.jsonBody (Basics.identity config.body)
        , timeout = Nothing
        }


{-| Edit webhook
-}
editWebhook :
    { authorization : { oauth2 : String }
    , toMsg :
        Result (OpenApi.Common.Error PatreonApi.Types.EditWebhook_Error String) PatreonApi.Types.WebhookResponse
        -> msg
    , body : Json.Encode.Value
    , params : { id : String, user_Agent : String }
    }
    -> Cmd msg
editWebhook config =
    Http.request
        { url =
            Url.Builder.crossOrigin
                "https://patreon.com"
                [ "api", "oauth", "v2", "webhooks", config.params.id ]
                []
        , method = "PATCH"
        , headers =
            [ Http.header
                "authorization"
                ("Bearer " ++ config.authorization.oauth2)
            , Http.header "User-Agent" config.params.user_Agent
            ]
        , expect =
            OpenApi.Common.expectJsonCustom
                config.toMsg
                (Dict.fromList
                    [ ( "400"
                      , Json.Decode.map
                            PatreonApi.Types.EditWebhook_400
                            PatreonApi.Json.decodeStatusCode400
                      )
                    , ( "401"
                      , Json.Decode.map
                            PatreonApi.Types.EditWebhook_401
                            PatreonApi.Json.decodeStatusCode401
                      )
                    , ( "403"
                      , Json.Decode.map
                            PatreonApi.Types.EditWebhook_403
                            PatreonApi.Json.decodeStatusCode403
                      )
                    , ( "404"
                      , Json.Decode.map
                            PatreonApi.Types.EditWebhook_404
                            PatreonApi.Json.decodeStatusCode404
                      )
                    , ( "405"
                      , Json.Decode.map
                            PatreonApi.Types.EditWebhook_405
                            PatreonApi.Json.decodeStatusCode405
                      )
                    , ( "406"
                      , Json.Decode.map
                            PatreonApi.Types.EditWebhook_406
                            PatreonApi.Json.decodeStatusCode406
                      )
                    , ( "410"
                      , Json.Decode.map
                            PatreonApi.Types.EditWebhook_410
                            PatreonApi.Json.decodeStatusCode410
                      )
                    , ( "429"
                      , Json.Decode.map
                            PatreonApi.Types.EditWebhook_429
                            PatreonApi.Json.decodeStatusCode429
                      )
                    , ( "500"
                      , Json.Decode.map
                            PatreonApi.Types.EditWebhook_500
                            PatreonApi.Json.decodeStatusCode500
                      )
                    , ( "503"
                      , Json.Decode.map
                            PatreonApi.Types.EditWebhook_503
                            PatreonApi.Json.decodeStatusCode503
                      )
                    ]
                )
                PatreonApi.Json.decodeWebhookResponse
        , body = Http.jsonBody (Basics.identity config.body)
        , timeout = Nothing
        , tracker = Nothing
        }


{-| Edit webhook
-}
editWebhookTask :
    { authorization : { oauth2 : String }
    , body : Json.Encode.Value
    , params : { id : String, user_Agent : String }
    }
    -> Task.Task (OpenApi.Common.Error PatreonApi.Types.EditWebhook_Error String) PatreonApi.Types.WebhookResponse
editWebhookTask config =
    Http.task
        { url =
            Url.Builder.crossOrigin
                "https://patreon.com"
                [ "api", "oauth", "v2", "webhooks", config.params.id ]
                []
        , method = "PATCH"
        , headers =
            [ Http.header
                "authorization"
                ("Bearer " ++ config.authorization.oauth2)
            , Http.header "User-Agent" config.params.user_Agent
            ]
        , resolver =
            OpenApi.Common.jsonResolverCustom
                (Dict.fromList
                    [ ( "400"
                      , Json.Decode.map
                            PatreonApi.Types.EditWebhook_400
                            PatreonApi.Json.decodeStatusCode400
                      )
                    , ( "401"
                      , Json.Decode.map
                            PatreonApi.Types.EditWebhook_401
                            PatreonApi.Json.decodeStatusCode401
                      )
                    , ( "403"
                      , Json.Decode.map
                            PatreonApi.Types.EditWebhook_403
                            PatreonApi.Json.decodeStatusCode403
                      )
                    , ( "404"
                      , Json.Decode.map
                            PatreonApi.Types.EditWebhook_404
                            PatreonApi.Json.decodeStatusCode404
                      )
                    , ( "405"
                      , Json.Decode.map
                            PatreonApi.Types.EditWebhook_405
                            PatreonApi.Json.decodeStatusCode405
                      )
                    , ( "406"
                      , Json.Decode.map
                            PatreonApi.Types.EditWebhook_406
                            PatreonApi.Json.decodeStatusCode406
                      )
                    , ( "410"
                      , Json.Decode.map
                            PatreonApi.Types.EditWebhook_410
                            PatreonApi.Json.decodeStatusCode410
                      )
                    , ( "429"
                      , Json.Decode.map
                            PatreonApi.Types.EditWebhook_429
                            PatreonApi.Json.decodeStatusCode429
                      )
                    , ( "500"
                      , Json.Decode.map
                            PatreonApi.Types.EditWebhook_500
                            PatreonApi.Json.decodeStatusCode500
                      )
                    , ( "503"
                      , Json.Decode.map
                            PatreonApi.Types.EditWebhook_503
                            PatreonApi.Json.decodeStatusCode503
                      )
                    ]
                )
                PatreonApi.Json.decodeWebhookResponse
        , body = Http.jsonBody (Basics.identity config.body)
        , timeout = Nothing
        }


{-| Get campaign
-}
getCampaign :
    { authorization : { oauth2 : String }
    , toMsg :
        Result (OpenApi.Common.Error PatreonApi.Types.GetCampaign_Error String) PatreonApi.Types.CampaignResponse
        -> msg
    , params :
        { include : Maybe (List String)
        , fields :
            Maybe
                { benefit : Maybe (List String)
                , campaign : Maybe (List String)
                , goal : Maybe (List String)
                , tier : Maybe (List String)
                , user : Maybe (List String)
                }
        , campaign_id : String
        , user_Agent : String
        }
    }
    -> Cmd msg
getCampaign config =
    Http.request
        { url =
            Url.Builder.crossOrigin
                "https://patreon.com"
                [ "api", "oauth", "v2", "campaigns", config.params.campaign_id ]
                (List.filterMap
                    Basics.identity
                    [ Maybe.map
                        (Url.Builder.string "include")
                        (Maybe.andThen
                            (\andThenUnpack ->
                                if List.isEmpty andThenUnpack then
                                    Nothing

                                else
                                    Just (String.join "," andThenUnpack)
                            )
                            config.params.include
                        )
                    , Maybe.map
                        (Url.Builder.string "fields.benefit")
                        (Maybe.andThen
                            (\andThenUnpack ->
                                if List.isEmpty andThenUnpack then
                                    Nothing

                                else
                                    Just (String.join "," andThenUnpack)
                            )
                            (Maybe.andThen .benefit config.params.fields)
                        )
                    , Maybe.map
                        (Url.Builder.string "fields.campaign")
                        (Maybe.andThen
                            (\andThenUnpack ->
                                if List.isEmpty andThenUnpack then
                                    Nothing

                                else
                                    Just (String.join "," andThenUnpack)
                            )
                            (Maybe.andThen .campaign config.params.fields)
                        )
                    , Maybe.map
                        (Url.Builder.string "fields.goal")
                        (Maybe.andThen
                            (\andThenUnpack ->
                                if List.isEmpty andThenUnpack then
                                    Nothing

                                else
                                    Just (String.join "," andThenUnpack)
                            )
                            (Maybe.andThen .goal config.params.fields)
                        )
                    , Maybe.map
                        (Url.Builder.string "fields.tier")
                        (Maybe.andThen
                            (\andThenUnpack ->
                                if List.isEmpty andThenUnpack then
                                    Nothing

                                else
                                    Just (String.join "," andThenUnpack)
                            )
                            (Maybe.andThen .tier config.params.fields)
                        )
                    , Maybe.map
                        (Url.Builder.string "fields.user")
                        (Maybe.andThen
                            (\andThenUnpack ->
                                if List.isEmpty andThenUnpack then
                                    Nothing

                                else
                                    Just (String.join "," andThenUnpack)
                            )
                            (Maybe.andThen .user config.params.fields)
                        )
                    ]
                )
        , method = "GET"
        , headers =
            [ Http.header
                "authorization"
                ("Bearer " ++ config.authorization.oauth2)
            , Http.header "User-Agent" config.params.user_Agent
            ]
        , expect =
            OpenApi.Common.expectJsonCustom
                config.toMsg
                (Dict.fromList
                    [ ( "400"
                      , Json.Decode.map
                            PatreonApi.Types.GetCampaign_400
                            PatreonApi.Json.decodeStatusCode400
                      )
                    , ( "401"
                      , Json.Decode.map
                            PatreonApi.Types.GetCampaign_401
                            PatreonApi.Json.decodeStatusCode401
                      )
                    , ( "403"
                      , Json.Decode.map
                            PatreonApi.Types.GetCampaign_403
                            PatreonApi.Json.decodeStatusCode403
                      )
                    , ( "404"
                      , Json.Decode.map
                            PatreonApi.Types.GetCampaign_404
                            PatreonApi.Json.decodeStatusCode404
                      )
                    , ( "405"
                      , Json.Decode.map
                            PatreonApi.Types.GetCampaign_405
                            PatreonApi.Json.decodeStatusCode405
                      )
                    , ( "406"
                      , Json.Decode.map
                            PatreonApi.Types.GetCampaign_406
                            PatreonApi.Json.decodeStatusCode406
                      )
                    , ( "410"
                      , Json.Decode.map
                            PatreonApi.Types.GetCampaign_410
                            PatreonApi.Json.decodeStatusCode410
                      )
                    , ( "429"
                      , Json.Decode.map
                            PatreonApi.Types.GetCampaign_429
                            PatreonApi.Json.decodeStatusCode429
                      )
                    , ( "500"
                      , Json.Decode.map
                            PatreonApi.Types.GetCampaign_500
                            PatreonApi.Json.decodeStatusCode500
                      )
                    , ( "503"
                      , Json.Decode.map
                            PatreonApi.Types.GetCampaign_503
                            PatreonApi.Json.decodeStatusCode503
                      )
                    ]
                )
                PatreonApi.Json.decodeCampaignResponse
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        }


{-| Get campaign members
-}
getCampaignMembers :
    { authorization : { oauth2 : String }
    , toMsg :
        Result (OpenApi.Common.Error PatreonApi.Types.GetCampaignMembers_Error String) PatreonApi.Types.MembersResponse
        -> msg
    , params :
        { include : Maybe (List String)
        , fields :
            Maybe
                { address : Maybe (List String)
                , campaign : Maybe (List String)
                , member : Maybe (List String)
                , pledge_event : Maybe (List String)
                , tier : Maybe (List String)
                , user : Maybe (List String)
                }
        , sort : Maybe String
        , page : Maybe { count : Maybe Float, cursor : Maybe String }
        , campaign_id : String
        , user_Agent : String
        }
    }
    -> Cmd msg
getCampaignMembers config =
    Http.request
        { url =
            Url.Builder.crossOrigin
                "https://patreon.com"
                [ "api"
                , "oauth"
                , "v2"
                , "campaigns"
                , config.params.campaign_id
                , "members"
                ]
                (List.filterMap
                    Basics.identity
                    [ Maybe.map
                        (Url.Builder.string "include")
                        (Maybe.andThen
                            (\andThenUnpack ->
                                if List.isEmpty andThenUnpack then
                                    Nothing

                                else
                                    Just (String.join "," andThenUnpack)
                            )
                            config.params.include
                        )
                    , Maybe.map
                        (Url.Builder.string "fields.address")
                        (Maybe.andThen
                            (\andThenUnpack ->
                                if List.isEmpty andThenUnpack then
                                    Nothing

                                else
                                    Just (String.join "," andThenUnpack)
                            )
                            (Maybe.andThen .address config.params.fields)
                        )
                    , Maybe.map
                        (Url.Builder.string "fields.campaign")
                        (Maybe.andThen
                            (\andThenUnpack ->
                                if List.isEmpty andThenUnpack then
                                    Nothing

                                else
                                    Just (String.join "," andThenUnpack)
                            )
                            (Maybe.andThen .campaign config.params.fields)
                        )
                    , Maybe.map
                        (Url.Builder.string "fields.member")
                        (Maybe.andThen
                            (\andThenUnpack ->
                                if List.isEmpty andThenUnpack then
                                    Nothing

                                else
                                    Just (String.join "," andThenUnpack)
                            )
                            (Maybe.andThen .member config.params.fields)
                        )
                    , Maybe.map
                        (Url.Builder.string "fields.pledge-event")
                        (Maybe.andThen
                            (\andThenUnpack ->
                                if List.isEmpty andThenUnpack then
                                    Nothing

                                else
                                    Just (String.join "," andThenUnpack)
                            )
                            (Maybe.andThen .pledge_event config.params.fields)
                        )
                    , Maybe.map
                        (Url.Builder.string "fields.tier")
                        (Maybe.andThen
                            (\andThenUnpack ->
                                if List.isEmpty andThenUnpack then
                                    Nothing

                                else
                                    Just (String.join "," andThenUnpack)
                            )
                            (Maybe.andThen .tier config.params.fields)
                        )
                    , Maybe.map
                        (Url.Builder.string "fields.user")
                        (Maybe.andThen
                            (\andThenUnpack ->
                                if List.isEmpty andThenUnpack then
                                    Nothing

                                else
                                    Just (String.join "," andThenUnpack)
                            )
                            (Maybe.andThen .user config.params.fields)
                        )
                    , Maybe.map (Url.Builder.string "sort") config.params.sort
                    , Maybe.map
                        (Url.Builder.string "page.count")
                        (Maybe.map
                            String.fromFloat
                            (Maybe.andThen .count config.params.page)
                        )
                    , Maybe.map
                        (Url.Builder.string "page.cursor")
                        (Maybe.andThen .cursor config.params.page)
                    ]
                )
        , method = "GET"
        , headers =
            [ Http.header
                "authorization"
                ("Bearer " ++ config.authorization.oauth2)
            , Http.header "User-Agent" config.params.user_Agent
            ]
        , expect =
            OpenApi.Common.expectJsonCustom
                config.toMsg
                (Dict.fromList
                    [ ( "400"
                      , Json.Decode.map
                            PatreonApi.Types.GetCampaignMembers_400
                            PatreonApi.Json.decodeStatusCode400
                      )
                    , ( "401"
                      , Json.Decode.map
                            PatreonApi.Types.GetCampaignMembers_401
                            PatreonApi.Json.decodeStatusCode401
                      )
                    , ( "403"
                      , Json.Decode.map
                            PatreonApi.Types.GetCampaignMembers_403
                            PatreonApi.Json.decodeStatusCode403
                      )
                    , ( "404"
                      , Json.Decode.map
                            PatreonApi.Types.GetCampaignMembers_404
                            PatreonApi.Json.decodeStatusCode404
                      )
                    , ( "405"
                      , Json.Decode.map
                            PatreonApi.Types.GetCampaignMembers_405
                            PatreonApi.Json.decodeStatusCode405
                      )
                    , ( "406"
                      , Json.Decode.map
                            PatreonApi.Types.GetCampaignMembers_406
                            PatreonApi.Json.decodeStatusCode406
                      )
                    , ( "410"
                      , Json.Decode.map
                            PatreonApi.Types.GetCampaignMembers_410
                            PatreonApi.Json.decodeStatusCode410
                      )
                    , ( "429"
                      , Json.Decode.map
                            PatreonApi.Types.GetCampaignMembers_429
                            PatreonApi.Json.decodeStatusCode429
                      )
                    , ( "500"
                      , Json.Decode.map
                            PatreonApi.Types.GetCampaignMembers_500
                            PatreonApi.Json.decodeStatusCode500
                      )
                    , ( "503"
                      , Json.Decode.map
                            PatreonApi.Types.GetCampaignMembers_503
                            PatreonApi.Json.decodeStatusCode503
                      )
                    ]
                )
                PatreonApi.Json.decodeMembersResponse
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        }


{-| Get campaign members
-}
getCampaignMembersTask :
    { authorization : { oauth2 : String }
    , params :
        { include : Maybe (List String)
        , fields :
            Maybe
                { address : Maybe (List String)
                , campaign : Maybe (List String)
                , member : Maybe (List String)
                , pledge_event : Maybe (List String)
                , tier : Maybe (List String)
                , user : Maybe (List String)
                }
        , sort : Maybe String
        , page : Maybe { count : Maybe Float, cursor : Maybe String }
        , campaign_id : String
        , user_Agent : String
        }
    }
    -> Task.Task (OpenApi.Common.Error PatreonApi.Types.GetCampaignMembers_Error String) PatreonApi.Types.MembersResponse
getCampaignMembersTask config =
    Http.task
        { url =
            Url.Builder.crossOrigin
                "https://patreon.com"
                [ "api"
                , "oauth"
                , "v2"
                , "campaigns"
                , config.params.campaign_id
                , "members"
                ]
                (List.filterMap
                    Basics.identity
                    [ Maybe.map
                        (Url.Builder.string "include")
                        (Maybe.andThen
                            (\andThenUnpack ->
                                if List.isEmpty andThenUnpack then
                                    Nothing

                                else
                                    Just (String.join "," andThenUnpack)
                            )
                            config.params.include
                        )
                    , Maybe.map
                        (Url.Builder.string "fields.address")
                        (Maybe.andThen
                            (\andThenUnpack ->
                                if List.isEmpty andThenUnpack then
                                    Nothing

                                else
                                    Just (String.join "," andThenUnpack)
                            )
                            (Maybe.andThen .address config.params.fields)
                        )
                    , Maybe.map
                        (Url.Builder.string "fields.campaign")
                        (Maybe.andThen
                            (\andThenUnpack ->
                                if List.isEmpty andThenUnpack then
                                    Nothing

                                else
                                    Just (String.join "," andThenUnpack)
                            )
                            (Maybe.andThen .campaign config.params.fields)
                        )
                    , Maybe.map
                        (Url.Builder.string "fields.member")
                        (Maybe.andThen
                            (\andThenUnpack ->
                                if List.isEmpty andThenUnpack then
                                    Nothing

                                else
                                    Just (String.join "," andThenUnpack)
                            )
                            (Maybe.andThen .member config.params.fields)
                        )
                    , Maybe.map
                        (Url.Builder.string "fields.pledge-event")
                        (Maybe.andThen
                            (\andThenUnpack ->
                                if List.isEmpty andThenUnpack then
                                    Nothing

                                else
                                    Just (String.join "," andThenUnpack)
                            )
                            (Maybe.andThen .pledge_event config.params.fields)
                        )
                    , Maybe.map
                        (Url.Builder.string "fields.tier")
                        (Maybe.andThen
                            (\andThenUnpack ->
                                if List.isEmpty andThenUnpack then
                                    Nothing

                                else
                                    Just (String.join "," andThenUnpack)
                            )
                            (Maybe.andThen .tier config.params.fields)
                        )
                    , Maybe.map
                        (Url.Builder.string "fields.user")
                        (Maybe.andThen
                            (\andThenUnpack ->
                                if List.isEmpty andThenUnpack then
                                    Nothing

                                else
                                    Just (String.join "," andThenUnpack)
                            )
                            (Maybe.andThen .user config.params.fields)
                        )
                    , Maybe.map (Url.Builder.string "sort") config.params.sort
                    , Maybe.map
                        (Url.Builder.string "page.count")
                        (Maybe.map
                            String.fromFloat
                            (Maybe.andThen .count config.params.page)
                        )
                    , Maybe.map
                        (Url.Builder.string "page.cursor")
                        (Maybe.andThen .cursor config.params.page)
                    ]
                )
        , method = "GET"
        , headers =
            [ Http.header
                "authorization"
                ("Bearer " ++ config.authorization.oauth2)
            , Http.header "User-Agent" config.params.user_Agent
            ]
        , resolver =
            OpenApi.Common.jsonResolverCustom
                (Dict.fromList
                    [ ( "400"
                      , Json.Decode.map
                            PatreonApi.Types.GetCampaignMembers_400
                            PatreonApi.Json.decodeStatusCode400
                      )
                    , ( "401"
                      , Json.Decode.map
                            PatreonApi.Types.GetCampaignMembers_401
                            PatreonApi.Json.decodeStatusCode401
                      )
                    , ( "403"
                      , Json.Decode.map
                            PatreonApi.Types.GetCampaignMembers_403
                            PatreonApi.Json.decodeStatusCode403
                      )
                    , ( "404"
                      , Json.Decode.map
                            PatreonApi.Types.GetCampaignMembers_404
                            PatreonApi.Json.decodeStatusCode404
                      )
                    , ( "405"
                      , Json.Decode.map
                            PatreonApi.Types.GetCampaignMembers_405
                            PatreonApi.Json.decodeStatusCode405
                      )
                    , ( "406"
                      , Json.Decode.map
                            PatreonApi.Types.GetCampaignMembers_406
                            PatreonApi.Json.decodeStatusCode406
                      )
                    , ( "410"
                      , Json.Decode.map
                            PatreonApi.Types.GetCampaignMembers_410
                            PatreonApi.Json.decodeStatusCode410
                      )
                    , ( "429"
                      , Json.Decode.map
                            PatreonApi.Types.GetCampaignMembers_429
                            PatreonApi.Json.decodeStatusCode429
                      )
                    , ( "500"
                      , Json.Decode.map
                            PatreonApi.Types.GetCampaignMembers_500
                            PatreonApi.Json.decodeStatusCode500
                      )
                    , ( "503"
                      , Json.Decode.map
                            PatreonApi.Types.GetCampaignMembers_503
                            PatreonApi.Json.decodeStatusCode503
                      )
                    ]
                )
                PatreonApi.Json.decodeMembersResponse
        , body = Http.emptyBody
        , timeout = Nothing
        }


{-| Get campaign posts
-}
getCampaignPosts :
    { authorization : { oauth2 : String }
    , toMsg :
        Result (OpenApi.Common.Error PatreonApi.Types.GetCampaignPosts_Error String) PatreonApi.Types.PostsResponse
        -> msg
    , params :
        { include : Maybe (List String)
        , fields :
            Maybe
                { campaign : Maybe (List String)
                , post : Maybe (List String)
                , user : Maybe (List String)
                }
        , sort : Maybe String
        , page : Maybe { count : Maybe Float, cursor : Maybe String }
        , campaign_id : String
        , user_Agent : String
        }
    }
    -> Cmd msg
getCampaignPosts config =
    Http.request
        { url =
            Url.Builder.crossOrigin
                "https://patreon.com"
                [ "api"
                , "oauth"
                , "v2"
                , "campaigns"
                , config.params.campaign_id
                , "posts"
                ]
                (List.filterMap
                    Basics.identity
                    [ Maybe.map
                        (Url.Builder.string "include")
                        (Maybe.andThen
                            (\andThenUnpack ->
                                if List.isEmpty andThenUnpack then
                                    Nothing

                                else
                                    Just (String.join "," andThenUnpack)
                            )
                            config.params.include
                        )
                    , Maybe.map
                        (Url.Builder.string "fields.campaign")
                        (Maybe.andThen
                            (\andThenUnpack ->
                                if List.isEmpty andThenUnpack then
                                    Nothing

                                else
                                    Just (String.join "," andThenUnpack)
                            )
                            (Maybe.andThen .campaign config.params.fields)
                        )
                    , Maybe.map
                        (Url.Builder.string "fields.post")
                        (Maybe.andThen
                            (\andThenUnpack ->
                                if List.isEmpty andThenUnpack then
                                    Nothing

                                else
                                    Just (String.join "," andThenUnpack)
                            )
                            (Maybe.andThen .post config.params.fields)
                        )
                    , Maybe.map
                        (Url.Builder.string "fields.user")
                        (Maybe.andThen
                            (\andThenUnpack ->
                                if List.isEmpty andThenUnpack then
                                    Nothing

                                else
                                    Just (String.join "," andThenUnpack)
                            )
                            (Maybe.andThen .user config.params.fields)
                        )
                    , Maybe.map (Url.Builder.string "sort") config.params.sort
                    , Maybe.map
                        (Url.Builder.string "page.count")
                        (Maybe.map
                            String.fromFloat
                            (Maybe.andThen .count config.params.page)
                        )
                    , Maybe.map
                        (Url.Builder.string "page.cursor")
                        (Maybe.andThen .cursor config.params.page)
                    ]
                )
        , method = "GET"
        , headers =
            [ Http.header
                "authorization"
                ("Bearer " ++ config.authorization.oauth2)
            , Http.header "User-Agent" config.params.user_Agent
            ]
        , expect =
            OpenApi.Common.expectJsonCustom
                config.toMsg
                (Dict.fromList
                    [ ( "400"
                      , Json.Decode.map
                            PatreonApi.Types.GetCampaignPosts_400
                            PatreonApi.Json.decodeStatusCode400
                      )
                    , ( "401"
                      , Json.Decode.map
                            PatreonApi.Types.GetCampaignPosts_401
                            PatreonApi.Json.decodeStatusCode401
                      )
                    , ( "403"
                      , Json.Decode.map
                            PatreonApi.Types.GetCampaignPosts_403
                            PatreonApi.Json.decodeStatusCode403
                      )
                    , ( "404"
                      , Json.Decode.map
                            PatreonApi.Types.GetCampaignPosts_404
                            PatreonApi.Json.decodeStatusCode404
                      )
                    , ( "405"
                      , Json.Decode.map
                            PatreonApi.Types.GetCampaignPosts_405
                            PatreonApi.Json.decodeStatusCode405
                      )
                    , ( "406"
                      , Json.Decode.map
                            PatreonApi.Types.GetCampaignPosts_406
                            PatreonApi.Json.decodeStatusCode406
                      )
                    , ( "410"
                      , Json.Decode.map
                            PatreonApi.Types.GetCampaignPosts_410
                            PatreonApi.Json.decodeStatusCode410
                      )
                    , ( "429"
                      , Json.Decode.map
                            PatreonApi.Types.GetCampaignPosts_429
                            PatreonApi.Json.decodeStatusCode429
                      )
                    , ( "500"
                      , Json.Decode.map
                            PatreonApi.Types.GetCampaignPosts_500
                            PatreonApi.Json.decodeStatusCode500
                      )
                    , ( "503"
                      , Json.Decode.map
                            PatreonApi.Types.GetCampaignPosts_503
                            PatreonApi.Json.decodeStatusCode503
                      )
                    ]
                )
                PatreonApi.Json.decodePostsResponse
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        }


{-| Get campaign posts
-}
getCampaignPostsTask :
    { authorization : { oauth2 : String }
    , params :
        { include : Maybe (List String)
        , fields :
            Maybe
                { campaign : Maybe (List String)
                , post : Maybe (List String)
                , user : Maybe (List String)
                }
        , sort : Maybe String
        , page : Maybe { count : Maybe Float, cursor : Maybe String }
        , campaign_id : String
        , user_Agent : String
        }
    }
    -> Task.Task (OpenApi.Common.Error PatreonApi.Types.GetCampaignPosts_Error String) PatreonApi.Types.PostsResponse
getCampaignPostsTask config =
    Http.task
        { url =
            Url.Builder.crossOrigin
                "https://patreon.com"
                [ "api"
                , "oauth"
                , "v2"
                , "campaigns"
                , config.params.campaign_id
                , "posts"
                ]
                (List.filterMap
                    Basics.identity
                    [ Maybe.map
                        (Url.Builder.string "include")
                        (Maybe.andThen
                            (\andThenUnpack ->
                                if List.isEmpty andThenUnpack then
                                    Nothing

                                else
                                    Just (String.join "," andThenUnpack)
                            )
                            config.params.include
                        )
                    , Maybe.map
                        (Url.Builder.string "fields.campaign")
                        (Maybe.andThen
                            (\andThenUnpack ->
                                if List.isEmpty andThenUnpack then
                                    Nothing

                                else
                                    Just (String.join "," andThenUnpack)
                            )
                            (Maybe.andThen .campaign config.params.fields)
                        )
                    , Maybe.map
                        (Url.Builder.string "fields.post")
                        (Maybe.andThen
                            (\andThenUnpack ->
                                if List.isEmpty andThenUnpack then
                                    Nothing

                                else
                                    Just (String.join "," andThenUnpack)
                            )
                            (Maybe.andThen .post config.params.fields)
                        )
                    , Maybe.map
                        (Url.Builder.string "fields.user")
                        (Maybe.andThen
                            (\andThenUnpack ->
                                if List.isEmpty andThenUnpack then
                                    Nothing

                                else
                                    Just (String.join "," andThenUnpack)
                            )
                            (Maybe.andThen .user config.params.fields)
                        )
                    , Maybe.map (Url.Builder.string "sort") config.params.sort
                    , Maybe.map
                        (Url.Builder.string "page.count")
                        (Maybe.map
                            String.fromFloat
                            (Maybe.andThen .count config.params.page)
                        )
                    , Maybe.map
                        (Url.Builder.string "page.cursor")
                        (Maybe.andThen .cursor config.params.page)
                    ]
                )
        , method = "GET"
        , headers =
            [ Http.header
                "authorization"
                ("Bearer " ++ config.authorization.oauth2)
            , Http.header "User-Agent" config.params.user_Agent
            ]
        , resolver =
            OpenApi.Common.jsonResolverCustom
                (Dict.fromList
                    [ ( "400"
                      , Json.Decode.map
                            PatreonApi.Types.GetCampaignPosts_400
                            PatreonApi.Json.decodeStatusCode400
                      )
                    , ( "401"
                      , Json.Decode.map
                            PatreonApi.Types.GetCampaignPosts_401
                            PatreonApi.Json.decodeStatusCode401
                      )
                    , ( "403"
                      , Json.Decode.map
                            PatreonApi.Types.GetCampaignPosts_403
                            PatreonApi.Json.decodeStatusCode403
                      )
                    , ( "404"
                      , Json.Decode.map
                            PatreonApi.Types.GetCampaignPosts_404
                            PatreonApi.Json.decodeStatusCode404
                      )
                    , ( "405"
                      , Json.Decode.map
                            PatreonApi.Types.GetCampaignPosts_405
                            PatreonApi.Json.decodeStatusCode405
                      )
                    , ( "406"
                      , Json.Decode.map
                            PatreonApi.Types.GetCampaignPosts_406
                            PatreonApi.Json.decodeStatusCode406
                      )
                    , ( "410"
                      , Json.Decode.map
                            PatreonApi.Types.GetCampaignPosts_410
                            PatreonApi.Json.decodeStatusCode410
                      )
                    , ( "429"
                      , Json.Decode.map
                            PatreonApi.Types.GetCampaignPosts_429
                            PatreonApi.Json.decodeStatusCode429
                      )
                    , ( "500"
                      , Json.Decode.map
                            PatreonApi.Types.GetCampaignPosts_500
                            PatreonApi.Json.decodeStatusCode500
                      )
                    , ( "503"
                      , Json.Decode.map
                            PatreonApi.Types.GetCampaignPosts_503
                            PatreonApi.Json.decodeStatusCode503
                      )
                    ]
                )
                PatreonApi.Json.decodePostsResponse
        , body = Http.emptyBody
        , timeout = Nothing
        }


{-| Get campaign
-}
getCampaignTask :
    { authorization : { oauth2 : String }
    , params :
        { include : Maybe (List String)
        , fields :
            Maybe
                { benefit : Maybe (List String)
                , campaign : Maybe (List String)
                , goal : Maybe (List String)
                , tier : Maybe (List String)
                , user : Maybe (List String)
                }
        , campaign_id : String
        , user_Agent : String
        }
    }
    -> Task.Task (OpenApi.Common.Error PatreonApi.Types.GetCampaign_Error String) PatreonApi.Types.CampaignResponse
getCampaignTask config =
    Http.task
        { url =
            Url.Builder.crossOrigin
                "https://patreon.com"
                [ "api", "oauth", "v2", "campaigns", config.params.campaign_id ]
                (List.filterMap
                    Basics.identity
                    [ Maybe.map
                        (Url.Builder.string "include")
                        (Maybe.andThen
                            (\andThenUnpack ->
                                if List.isEmpty andThenUnpack then
                                    Nothing

                                else
                                    Just (String.join "," andThenUnpack)
                            )
                            config.params.include
                        )
                    , Maybe.map
                        (Url.Builder.string "fields.benefit")
                        (Maybe.andThen
                            (\andThenUnpack ->
                                if List.isEmpty andThenUnpack then
                                    Nothing

                                else
                                    Just (String.join "," andThenUnpack)
                            )
                            (Maybe.andThen .benefit config.params.fields)
                        )
                    , Maybe.map
                        (Url.Builder.string "fields.campaign")
                        (Maybe.andThen
                            (\andThenUnpack ->
                                if List.isEmpty andThenUnpack then
                                    Nothing

                                else
                                    Just (String.join "," andThenUnpack)
                            )
                            (Maybe.andThen .campaign config.params.fields)
                        )
                    , Maybe.map
                        (Url.Builder.string "fields.goal")
                        (Maybe.andThen
                            (\andThenUnpack ->
                                if List.isEmpty andThenUnpack then
                                    Nothing

                                else
                                    Just (String.join "," andThenUnpack)
                            )
                            (Maybe.andThen .goal config.params.fields)
                        )
                    , Maybe.map
                        (Url.Builder.string "fields.tier")
                        (Maybe.andThen
                            (\andThenUnpack ->
                                if List.isEmpty andThenUnpack then
                                    Nothing

                                else
                                    Just (String.join "," andThenUnpack)
                            )
                            (Maybe.andThen .tier config.params.fields)
                        )
                    , Maybe.map
                        (Url.Builder.string "fields.user")
                        (Maybe.andThen
                            (\andThenUnpack ->
                                if List.isEmpty andThenUnpack then
                                    Nothing

                                else
                                    Just (String.join "," andThenUnpack)
                            )
                            (Maybe.andThen .user config.params.fields)
                        )
                    ]
                )
        , method = "GET"
        , headers =
            [ Http.header
                "authorization"
                ("Bearer " ++ config.authorization.oauth2)
            , Http.header "User-Agent" config.params.user_Agent
            ]
        , resolver =
            OpenApi.Common.jsonResolverCustom
                (Dict.fromList
                    [ ( "400"
                      , Json.Decode.map
                            PatreonApi.Types.GetCampaign_400
                            PatreonApi.Json.decodeStatusCode400
                      )
                    , ( "401"
                      , Json.Decode.map
                            PatreonApi.Types.GetCampaign_401
                            PatreonApi.Json.decodeStatusCode401
                      )
                    , ( "403"
                      , Json.Decode.map
                            PatreonApi.Types.GetCampaign_403
                            PatreonApi.Json.decodeStatusCode403
                      )
                    , ( "404"
                      , Json.Decode.map
                            PatreonApi.Types.GetCampaign_404
                            PatreonApi.Json.decodeStatusCode404
                      )
                    , ( "405"
                      , Json.Decode.map
                            PatreonApi.Types.GetCampaign_405
                            PatreonApi.Json.decodeStatusCode405
                      )
                    , ( "406"
                      , Json.Decode.map
                            PatreonApi.Types.GetCampaign_406
                            PatreonApi.Json.decodeStatusCode406
                      )
                    , ( "410"
                      , Json.Decode.map
                            PatreonApi.Types.GetCampaign_410
                            PatreonApi.Json.decodeStatusCode410
                      )
                    , ( "429"
                      , Json.Decode.map
                            PatreonApi.Types.GetCampaign_429
                            PatreonApi.Json.decodeStatusCode429
                      )
                    , ( "500"
                      , Json.Decode.map
                            PatreonApi.Types.GetCampaign_500
                            PatreonApi.Json.decodeStatusCode500
                      )
                    , ( "503"
                      , Json.Decode.map
                            PatreonApi.Types.GetCampaign_503
                            PatreonApi.Json.decodeStatusCode503
                      )
                    ]
                )
                PatreonApi.Json.decodeCampaignResponse
        , body = Http.emptyBody
        , timeout = Nothing
        }


{-| Get campaigns
-}
getCampaigns :
    { authorization : { oauth2 : String }
    , toMsg :
        Result (OpenApi.Common.Error PatreonApi.Types.GetCampaigns_Error String) PatreonApi.Types.CampaignsResponse
        -> msg
    , params :
        { include : Maybe (List String)
        , fields :
            Maybe
                { benefit : Maybe (List String)
                , campaign : Maybe (List String)
                , goal : Maybe (List String)
                , tier : Maybe (List String)
                , user : Maybe (List String)
                }
        , sort : Maybe String
        , page : Maybe { count : Maybe Float, cursor : Maybe String }
        , user_Agent : String
        }
    }
    -> Cmd msg
getCampaigns config =
    Http.request
        { url =
            Url.Builder.crossOrigin
                "https://patreon.com"
                [ "api", "oauth", "v2", "campaigns" ]
                (List.filterMap
                    Basics.identity
                    [ Maybe.map
                        (Url.Builder.string "include")
                        (Maybe.andThen
                            (\andThenUnpack ->
                                if List.isEmpty andThenUnpack then
                                    Nothing

                                else
                                    Just (String.join "," andThenUnpack)
                            )
                            config.params.include
                        )
                    , Maybe.map
                        (Url.Builder.string "fields.benefit")
                        (Maybe.andThen
                            (\andThenUnpack ->
                                if List.isEmpty andThenUnpack then
                                    Nothing

                                else
                                    Just (String.join "," andThenUnpack)
                            )
                            (Maybe.andThen .benefit config.params.fields)
                        )
                    , Maybe.map
                        (Url.Builder.string "fields.campaign")
                        (Maybe.andThen
                            (\andThenUnpack ->
                                if List.isEmpty andThenUnpack then
                                    Nothing

                                else
                                    Just (String.join "," andThenUnpack)
                            )
                            (Maybe.andThen .campaign config.params.fields)
                        )
                    , Maybe.map
                        (Url.Builder.string "fields.goal")
                        (Maybe.andThen
                            (\andThenUnpack ->
                                if List.isEmpty andThenUnpack then
                                    Nothing

                                else
                                    Just (String.join "," andThenUnpack)
                            )
                            (Maybe.andThen .goal config.params.fields)
                        )
                    , Maybe.map
                        (Url.Builder.string "fields.tier")
                        (Maybe.andThen
                            (\andThenUnpack ->
                                if List.isEmpty andThenUnpack then
                                    Nothing

                                else
                                    Just (String.join "," andThenUnpack)
                            )
                            (Maybe.andThen .tier config.params.fields)
                        )
                    , Maybe.map
                        (Url.Builder.string "fields.user")
                        (Maybe.andThen
                            (\andThenUnpack ->
                                if List.isEmpty andThenUnpack then
                                    Nothing

                                else
                                    Just (String.join "," andThenUnpack)
                            )
                            (Maybe.andThen .user config.params.fields)
                        )
                    , Maybe.map (Url.Builder.string "sort") config.params.sort
                    , Maybe.map
                        (Url.Builder.string "page.count")
                        (Maybe.map
                            String.fromFloat
                            (Maybe.andThen .count config.params.page)
                        )
                    , Maybe.map
                        (Url.Builder.string "page.cursor")
                        (Maybe.andThen .cursor config.params.page)
                    ]
                )
        , method = "GET"
        , headers =
            [ Http.header
                "authorization"
                ("Bearer " ++ config.authorization.oauth2)
            , Http.header "User-Agent" config.params.user_Agent
            ]
        , expect =
            OpenApi.Common.expectJsonCustom
                config.toMsg
                (Dict.fromList
                    [ ( "400"
                      , Json.Decode.map
                            PatreonApi.Types.GetCampaigns_400
                            PatreonApi.Json.decodeStatusCode400
                      )
                    , ( "401"
                      , Json.Decode.map
                            PatreonApi.Types.GetCampaigns_401
                            PatreonApi.Json.decodeStatusCode401
                      )
                    , ( "403"
                      , Json.Decode.map
                            PatreonApi.Types.GetCampaigns_403
                            PatreonApi.Json.decodeStatusCode403
                      )
                    , ( "404"
                      , Json.Decode.map
                            PatreonApi.Types.GetCampaigns_404
                            PatreonApi.Json.decodeStatusCode404
                      )
                    , ( "405"
                      , Json.Decode.map
                            PatreonApi.Types.GetCampaigns_405
                            PatreonApi.Json.decodeStatusCode405
                      )
                    , ( "406"
                      , Json.Decode.map
                            PatreonApi.Types.GetCampaigns_406
                            PatreonApi.Json.decodeStatusCode406
                      )
                    , ( "410"
                      , Json.Decode.map
                            PatreonApi.Types.GetCampaigns_410
                            PatreonApi.Json.decodeStatusCode410
                      )
                    , ( "429"
                      , Json.Decode.map
                            PatreonApi.Types.GetCampaigns_429
                            PatreonApi.Json.decodeStatusCode429
                      )
                    , ( "500"
                      , Json.Decode.map
                            PatreonApi.Types.GetCampaigns_500
                            PatreonApi.Json.decodeStatusCode500
                      )
                    , ( "503"
                      , Json.Decode.map
                            PatreonApi.Types.GetCampaigns_503
                            PatreonApi.Json.decodeStatusCode503
                      )
                    ]
                )
                PatreonApi.Json.decodeCampaignsResponse
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        }


{-| Get campaigns
-}
getCampaignsTask :
    { authorization : { oauth2 : String }
    , params :
        { include : Maybe (List String)
        , fields :
            Maybe
                { benefit : Maybe (List String)
                , campaign : Maybe (List String)
                , goal : Maybe (List String)
                , tier : Maybe (List String)
                , user : Maybe (List String)
                }
        , sort : Maybe String
        , page : Maybe { count : Maybe Float, cursor : Maybe String }
        , user_Agent : String
        }
    }
    -> Task.Task (OpenApi.Common.Error PatreonApi.Types.GetCampaigns_Error String) PatreonApi.Types.CampaignsResponse
getCampaignsTask config =
    Http.task
        { url =
            Url.Builder.crossOrigin
                "https://patreon.com"
                [ "api", "oauth", "v2", "campaigns" ]
                (List.filterMap
                    Basics.identity
                    [ Maybe.map
                        (Url.Builder.string "include")
                        (Maybe.andThen
                            (\andThenUnpack ->
                                if List.isEmpty andThenUnpack then
                                    Nothing

                                else
                                    Just (String.join "," andThenUnpack)
                            )
                            config.params.include
                        )
                    , Maybe.map
                        (Url.Builder.string "fields.benefit")
                        (Maybe.andThen
                            (\andThenUnpack ->
                                if List.isEmpty andThenUnpack then
                                    Nothing

                                else
                                    Just (String.join "," andThenUnpack)
                            )
                            (Maybe.andThen .benefit config.params.fields)
                        )
                    , Maybe.map
                        (Url.Builder.string "fields.campaign")
                        (Maybe.andThen
                            (\andThenUnpack ->
                                if List.isEmpty andThenUnpack then
                                    Nothing

                                else
                                    Just (String.join "," andThenUnpack)
                            )
                            (Maybe.andThen .campaign config.params.fields)
                        )
                    , Maybe.map
                        (Url.Builder.string "fields.goal")
                        (Maybe.andThen
                            (\andThenUnpack ->
                                if List.isEmpty andThenUnpack then
                                    Nothing

                                else
                                    Just (String.join "," andThenUnpack)
                            )
                            (Maybe.andThen .goal config.params.fields)
                        )
                    , Maybe.map
                        (Url.Builder.string "fields.tier")
                        (Maybe.andThen
                            (\andThenUnpack ->
                                if List.isEmpty andThenUnpack then
                                    Nothing

                                else
                                    Just (String.join "," andThenUnpack)
                            )
                            (Maybe.andThen .tier config.params.fields)
                        )
                    , Maybe.map
                        (Url.Builder.string "fields.user")
                        (Maybe.andThen
                            (\andThenUnpack ->
                                if List.isEmpty andThenUnpack then
                                    Nothing

                                else
                                    Just (String.join "," andThenUnpack)
                            )
                            (Maybe.andThen .user config.params.fields)
                        )
                    , Maybe.map (Url.Builder.string "sort") config.params.sort
                    , Maybe.map
                        (Url.Builder.string "page.count")
                        (Maybe.map
                            String.fromFloat
                            (Maybe.andThen .count config.params.page)
                        )
                    , Maybe.map
                        (Url.Builder.string "page.cursor")
                        (Maybe.andThen .cursor config.params.page)
                    ]
                )
        , method = "GET"
        , headers =
            [ Http.header
                "authorization"
                ("Bearer " ++ config.authorization.oauth2)
            , Http.header "User-Agent" config.params.user_Agent
            ]
        , resolver =
            OpenApi.Common.jsonResolverCustom
                (Dict.fromList
                    [ ( "400"
                      , Json.Decode.map
                            PatreonApi.Types.GetCampaigns_400
                            PatreonApi.Json.decodeStatusCode400
                      )
                    , ( "401"
                      , Json.Decode.map
                            PatreonApi.Types.GetCampaigns_401
                            PatreonApi.Json.decodeStatusCode401
                      )
                    , ( "403"
                      , Json.Decode.map
                            PatreonApi.Types.GetCampaigns_403
                            PatreonApi.Json.decodeStatusCode403
                      )
                    , ( "404"
                      , Json.Decode.map
                            PatreonApi.Types.GetCampaigns_404
                            PatreonApi.Json.decodeStatusCode404
                      )
                    , ( "405"
                      , Json.Decode.map
                            PatreonApi.Types.GetCampaigns_405
                            PatreonApi.Json.decodeStatusCode405
                      )
                    , ( "406"
                      , Json.Decode.map
                            PatreonApi.Types.GetCampaigns_406
                            PatreonApi.Json.decodeStatusCode406
                      )
                    , ( "410"
                      , Json.Decode.map
                            PatreonApi.Types.GetCampaigns_410
                            PatreonApi.Json.decodeStatusCode410
                      )
                    , ( "429"
                      , Json.Decode.map
                            PatreonApi.Types.GetCampaigns_429
                            PatreonApi.Json.decodeStatusCode429
                      )
                    , ( "500"
                      , Json.Decode.map
                            PatreonApi.Types.GetCampaigns_500
                            PatreonApi.Json.decodeStatusCode500
                      )
                    , ( "503"
                      , Json.Decode.map
                            PatreonApi.Types.GetCampaigns_503
                            PatreonApi.Json.decodeStatusCode503
                      )
                    ]
                )
                PatreonApi.Json.decodeCampaignsResponse
        , body = Http.emptyBody
        , timeout = Nothing
        }


{-| Get identity
-}
getIdentity :
    { authorization : { oauth2 : String }
    , toMsg :
        Result (OpenApi.Common.Error PatreonApi.Types.GetIdentity_Error String) PatreonApi.Types.UserResponse
        -> msg
    , params :
        { include : Maybe (List String)
        , fields :
            Maybe
                { campaign : Maybe (List String)
                , member : Maybe (List String)
                , user : Maybe (List String)
                }
        , user_Agent : String
        }
    }
    -> Cmd msg
getIdentity config =
    Http.request
        { url =
            Url.Builder.crossOrigin
                "https://patreon.com"
                [ "api", "oauth", "v2", "identity" ]
                (List.filterMap
                    Basics.identity
                    [ Maybe.map
                        (Url.Builder.string "include")
                        (Maybe.andThen
                            (\andThenUnpack ->
                                if List.isEmpty andThenUnpack then
                                    Nothing

                                else
                                    Just (String.join "," andThenUnpack)
                            )
                            config.params.include
                        )
                    , Maybe.map
                        (Url.Builder.string "fields.campaign")
                        (Maybe.andThen
                            (\andThenUnpack ->
                                if List.isEmpty andThenUnpack then
                                    Nothing

                                else
                                    Just (String.join "," andThenUnpack)
                            )
                            (Maybe.andThen .campaign config.params.fields)
                        )
                    , Maybe.map
                        (Url.Builder.string "fields.member")
                        (Maybe.andThen
                            (\andThenUnpack ->
                                if List.isEmpty andThenUnpack then
                                    Nothing

                                else
                                    Just (String.join "," andThenUnpack)
                            )
                            (Maybe.andThen .member config.params.fields)
                        )
                    , Maybe.map
                        (Url.Builder.string "fields.user")
                        (Maybe.andThen
                            (\andThenUnpack ->
                                if List.isEmpty andThenUnpack then
                                    Nothing

                                else
                                    Just (String.join "," andThenUnpack)
                            )
                            (Maybe.andThen .user config.params.fields)
                        )
                    ]
                )
        , method = "GET"
        , headers =
            [ Http.header
                "authorization"
                ("Bearer " ++ config.authorization.oauth2)
            , Http.header "User-Agent" config.params.user_Agent
            ]
        , expect =
            OpenApi.Common.expectJsonCustom
                config.toMsg
                (Dict.fromList
                    [ ( "400"
                      , Json.Decode.map
                            PatreonApi.Types.GetIdentity_400
                            PatreonApi.Json.decodeStatusCode400
                      )
                    , ( "401"
                      , Json.Decode.map
                            PatreonApi.Types.GetIdentity_401
                            PatreonApi.Json.decodeStatusCode401
                      )
                    , ( "403"
                      , Json.Decode.map
                            PatreonApi.Types.GetIdentity_403
                            PatreonApi.Json.decodeStatusCode403
                      )
                    , ( "404"
                      , Json.Decode.map
                            PatreonApi.Types.GetIdentity_404
                            PatreonApi.Json.decodeStatusCode404
                      )
                    , ( "405"
                      , Json.Decode.map
                            PatreonApi.Types.GetIdentity_405
                            PatreonApi.Json.decodeStatusCode405
                      )
                    , ( "406"
                      , Json.Decode.map
                            PatreonApi.Types.GetIdentity_406
                            PatreonApi.Json.decodeStatusCode406
                      )
                    , ( "410"
                      , Json.Decode.map
                            PatreonApi.Types.GetIdentity_410
                            PatreonApi.Json.decodeStatusCode410
                      )
                    , ( "429"
                      , Json.Decode.map
                            PatreonApi.Types.GetIdentity_429
                            PatreonApi.Json.decodeStatusCode429
                      )
                    , ( "500"
                      , Json.Decode.map
                            PatreonApi.Types.GetIdentity_500
                            PatreonApi.Json.decodeStatusCode500
                      )
                    , ( "503"
                      , Json.Decode.map
                            PatreonApi.Types.GetIdentity_503
                            PatreonApi.Json.decodeStatusCode503
                      )
                    ]
                )
                PatreonApi.Json.decodeUserResponse
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        }


{-| Get identity
-}
getIdentityTask :
    { authorization : { oauth2 : String }
    , params :
        { include : Maybe (List String)
        , fields :
            Maybe
                { campaign : Maybe (List String)
                , member : Maybe (List String)
                , user : Maybe (List String)
                }
        , user_Agent : String
        }
    }
    -> Task.Task (OpenApi.Common.Error PatreonApi.Types.GetIdentity_Error String) PatreonApi.Types.UserResponse
getIdentityTask config =
    Http.task
        { url =
            Url.Builder.crossOrigin
                "https://patreon.com"
                [ "api", "oauth", "v2", "identity" ]
                (List.filterMap
                    Basics.identity
                    [ Maybe.map
                        (Url.Builder.string "include")
                        (Maybe.andThen
                            (\andThenUnpack ->
                                if List.isEmpty andThenUnpack then
                                    Nothing

                                else
                                    Just (String.join "," andThenUnpack)
                            )
                            config.params.include
                        )
                    , Maybe.map
                        (Url.Builder.string "fields.campaign")
                        (Maybe.andThen
                            (\andThenUnpack ->
                                if List.isEmpty andThenUnpack then
                                    Nothing

                                else
                                    Just (String.join "," andThenUnpack)
                            )
                            (Maybe.andThen .campaign config.params.fields)
                        )
                    , Maybe.map
                        (Url.Builder.string "fields.member")
                        (Maybe.andThen
                            (\andThenUnpack ->
                                if List.isEmpty andThenUnpack then
                                    Nothing

                                else
                                    Just (String.join "," andThenUnpack)
                            )
                            (Maybe.andThen .member config.params.fields)
                        )
                    , Maybe.map
                        (Url.Builder.string "fields.user")
                        (Maybe.andThen
                            (\andThenUnpack ->
                                if List.isEmpty andThenUnpack then
                                    Nothing

                                else
                                    Just (String.join "," andThenUnpack)
                            )
                            (Maybe.andThen .user config.params.fields)
                        )
                    ]
                )
        , method = "GET"
        , headers =
            [ Http.header
                "authorization"
                ("Bearer " ++ config.authorization.oauth2)
            , Http.header "User-Agent" config.params.user_Agent
            ]
        , resolver =
            OpenApi.Common.jsonResolverCustom
                (Dict.fromList
                    [ ( "400"
                      , Json.Decode.map
                            PatreonApi.Types.GetIdentity_400
                            PatreonApi.Json.decodeStatusCode400
                      )
                    , ( "401"
                      , Json.Decode.map
                            PatreonApi.Types.GetIdentity_401
                            PatreonApi.Json.decodeStatusCode401
                      )
                    , ( "403"
                      , Json.Decode.map
                            PatreonApi.Types.GetIdentity_403
                            PatreonApi.Json.decodeStatusCode403
                      )
                    , ( "404"
                      , Json.Decode.map
                            PatreonApi.Types.GetIdentity_404
                            PatreonApi.Json.decodeStatusCode404
                      )
                    , ( "405"
                      , Json.Decode.map
                            PatreonApi.Types.GetIdentity_405
                            PatreonApi.Json.decodeStatusCode405
                      )
                    , ( "406"
                      , Json.Decode.map
                            PatreonApi.Types.GetIdentity_406
                            PatreonApi.Json.decodeStatusCode406
                      )
                    , ( "410"
                      , Json.Decode.map
                            PatreonApi.Types.GetIdentity_410
                            PatreonApi.Json.decodeStatusCode410
                      )
                    , ( "429"
                      , Json.Decode.map
                            PatreonApi.Types.GetIdentity_429
                            PatreonApi.Json.decodeStatusCode429
                      )
                    , ( "500"
                      , Json.Decode.map
                            PatreonApi.Types.GetIdentity_500
                            PatreonApi.Json.decodeStatusCode500
                      )
                    , ( "503"
                      , Json.Decode.map
                            PatreonApi.Types.GetIdentity_503
                            PatreonApi.Json.decodeStatusCode503
                      )
                    ]
                )
                PatreonApi.Json.decodeUserResponse
        , body = Http.emptyBody
        , timeout = Nothing
        }


{-| Get member
-}
getMember :
    { authorization : { oauth2 : String }
    , toMsg :
        Result (OpenApi.Common.Error PatreonApi.Types.GetMember_Error String) PatreonApi.Types.MemberResponse
        -> msg
    , params :
        { include : Maybe (List String)
        , fields :
            Maybe
                { address : Maybe (List String)
                , campaign : Maybe (List String)
                , member : Maybe (List String)
                , pledge_event : Maybe (List String)
                , tier : Maybe (List String)
                , user : Maybe (List String)
                }
        , id : String
        , user_Agent : String
        }
    }
    -> Cmd msg
getMember config =
    Http.request
        { url =
            Url.Builder.crossOrigin
                "https://patreon.com"
                [ "api", "oauth", "v2", "members", config.params.id ]
                (List.filterMap
                    Basics.identity
                    [ Maybe.map
                        (Url.Builder.string "include")
                        (Maybe.andThen
                            (\andThenUnpack ->
                                if List.isEmpty andThenUnpack then
                                    Nothing

                                else
                                    Just (String.join "," andThenUnpack)
                            )
                            config.params.include
                        )
                    , Maybe.map
                        (Url.Builder.string "fields.address")
                        (Maybe.andThen
                            (\andThenUnpack ->
                                if List.isEmpty andThenUnpack then
                                    Nothing

                                else
                                    Just (String.join "," andThenUnpack)
                            )
                            (Maybe.andThen .address config.params.fields)
                        )
                    , Maybe.map
                        (Url.Builder.string "fields.campaign")
                        (Maybe.andThen
                            (\andThenUnpack ->
                                if List.isEmpty andThenUnpack then
                                    Nothing

                                else
                                    Just (String.join "," andThenUnpack)
                            )
                            (Maybe.andThen .campaign config.params.fields)
                        )
                    , Maybe.map
                        (Url.Builder.string "fields.member")
                        (Maybe.andThen
                            (\andThenUnpack ->
                                if List.isEmpty andThenUnpack then
                                    Nothing

                                else
                                    Just (String.join "," andThenUnpack)
                            )
                            (Maybe.andThen .member config.params.fields)
                        )
                    , Maybe.map
                        (Url.Builder.string "fields.pledge-event")
                        (Maybe.andThen
                            (\andThenUnpack ->
                                if List.isEmpty andThenUnpack then
                                    Nothing

                                else
                                    Just (String.join "," andThenUnpack)
                            )
                            (Maybe.andThen .pledge_event config.params.fields)
                        )
                    , Maybe.map
                        (Url.Builder.string "fields.tier")
                        (Maybe.andThen
                            (\andThenUnpack ->
                                if List.isEmpty andThenUnpack then
                                    Nothing

                                else
                                    Just (String.join "," andThenUnpack)
                            )
                            (Maybe.andThen .tier config.params.fields)
                        )
                    , Maybe.map
                        (Url.Builder.string "fields.user")
                        (Maybe.andThen
                            (\andThenUnpack ->
                                if List.isEmpty andThenUnpack then
                                    Nothing

                                else
                                    Just (String.join "," andThenUnpack)
                            )
                            (Maybe.andThen .user config.params.fields)
                        )
                    ]
                )
        , method = "GET"
        , headers =
            [ Http.header
                "authorization"
                ("Bearer " ++ config.authorization.oauth2)
            , Http.header "User-Agent" config.params.user_Agent
            ]
        , expect =
            OpenApi.Common.expectJsonCustom
                config.toMsg
                (Dict.fromList
                    [ ( "400"
                      , Json.Decode.map
                            PatreonApi.Types.GetMember_400
                            PatreonApi.Json.decodeStatusCode400
                      )
                    , ( "401"
                      , Json.Decode.map
                            PatreonApi.Types.GetMember_401
                            PatreonApi.Json.decodeStatusCode401
                      )
                    , ( "403"
                      , Json.Decode.map
                            PatreonApi.Types.GetMember_403
                            PatreonApi.Json.decodeStatusCode403
                      )
                    , ( "404"
                      , Json.Decode.map
                            PatreonApi.Types.GetMember_404
                            PatreonApi.Json.decodeStatusCode404
                      )
                    , ( "405"
                      , Json.Decode.map
                            PatreonApi.Types.GetMember_405
                            PatreonApi.Json.decodeStatusCode405
                      )
                    , ( "406"
                      , Json.Decode.map
                            PatreonApi.Types.GetMember_406
                            PatreonApi.Json.decodeStatusCode406
                      )
                    , ( "410"
                      , Json.Decode.map
                            PatreonApi.Types.GetMember_410
                            PatreonApi.Json.decodeStatusCode410
                      )
                    , ( "429"
                      , Json.Decode.map
                            PatreonApi.Types.GetMember_429
                            PatreonApi.Json.decodeStatusCode429
                      )
                    , ( "500"
                      , Json.Decode.map
                            PatreonApi.Types.GetMember_500
                            PatreonApi.Json.decodeStatusCode500
                      )
                    , ( "503"
                      , Json.Decode.map
                            PatreonApi.Types.GetMember_503
                            PatreonApi.Json.decodeStatusCode503
                      )
                    ]
                )
                PatreonApi.Json.decodeMemberResponse
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        }


{-| Get member
-}
getMemberTask :
    { authorization : { oauth2 : String }
    , params :
        { include : Maybe (List String)
        , fields :
            Maybe
                { address : Maybe (List String)
                , campaign : Maybe (List String)
                , member : Maybe (List String)
                , pledge_event : Maybe (List String)
                , tier : Maybe (List String)
                , user : Maybe (List String)
                }
        , id : String
        , user_Agent : String
        }
    }
    -> Task.Task (OpenApi.Common.Error PatreonApi.Types.GetMember_Error String) PatreonApi.Types.MemberResponse
getMemberTask config =
    Http.task
        { url =
            Url.Builder.crossOrigin
                "https://patreon.com"
                [ "api", "oauth", "v2", "members", config.params.id ]
                (List.filterMap
                    Basics.identity
                    [ Maybe.map
                        (Url.Builder.string "include")
                        (Maybe.andThen
                            (\andThenUnpack ->
                                if List.isEmpty andThenUnpack then
                                    Nothing

                                else
                                    Just (String.join "," andThenUnpack)
                            )
                            config.params.include
                        )
                    , Maybe.map
                        (Url.Builder.string "fields.address")
                        (Maybe.andThen
                            (\andThenUnpack ->
                                if List.isEmpty andThenUnpack then
                                    Nothing

                                else
                                    Just (String.join "," andThenUnpack)
                            )
                            (Maybe.andThen .address config.params.fields)
                        )
                    , Maybe.map
                        (Url.Builder.string "fields.campaign")
                        (Maybe.andThen
                            (\andThenUnpack ->
                                if List.isEmpty andThenUnpack then
                                    Nothing

                                else
                                    Just (String.join "," andThenUnpack)
                            )
                            (Maybe.andThen .campaign config.params.fields)
                        )
                    , Maybe.map
                        (Url.Builder.string "fields.member")
                        (Maybe.andThen
                            (\andThenUnpack ->
                                if List.isEmpty andThenUnpack then
                                    Nothing

                                else
                                    Just (String.join "," andThenUnpack)
                            )
                            (Maybe.andThen .member config.params.fields)
                        )
                    , Maybe.map
                        (Url.Builder.string "fields.pledge-event")
                        (Maybe.andThen
                            (\andThenUnpack ->
                                if List.isEmpty andThenUnpack then
                                    Nothing

                                else
                                    Just (String.join "," andThenUnpack)
                            )
                            (Maybe.andThen .pledge_event config.params.fields)
                        )
                    , Maybe.map
                        (Url.Builder.string "fields.tier")
                        (Maybe.andThen
                            (\andThenUnpack ->
                                if List.isEmpty andThenUnpack then
                                    Nothing

                                else
                                    Just (String.join "," andThenUnpack)
                            )
                            (Maybe.andThen .tier config.params.fields)
                        )
                    , Maybe.map
                        (Url.Builder.string "fields.user")
                        (Maybe.andThen
                            (\andThenUnpack ->
                                if List.isEmpty andThenUnpack then
                                    Nothing

                                else
                                    Just (String.join "," andThenUnpack)
                            )
                            (Maybe.andThen .user config.params.fields)
                        )
                    ]
                )
        , method = "GET"
        , headers =
            [ Http.header
                "authorization"
                ("Bearer " ++ config.authorization.oauth2)
            , Http.header "User-Agent" config.params.user_Agent
            ]
        , resolver =
            OpenApi.Common.jsonResolverCustom
                (Dict.fromList
                    [ ( "400"
                      , Json.Decode.map
                            PatreonApi.Types.GetMember_400
                            PatreonApi.Json.decodeStatusCode400
                      )
                    , ( "401"
                      , Json.Decode.map
                            PatreonApi.Types.GetMember_401
                            PatreonApi.Json.decodeStatusCode401
                      )
                    , ( "403"
                      , Json.Decode.map
                            PatreonApi.Types.GetMember_403
                            PatreonApi.Json.decodeStatusCode403
                      )
                    , ( "404"
                      , Json.Decode.map
                            PatreonApi.Types.GetMember_404
                            PatreonApi.Json.decodeStatusCode404
                      )
                    , ( "405"
                      , Json.Decode.map
                            PatreonApi.Types.GetMember_405
                            PatreonApi.Json.decodeStatusCode405
                      )
                    , ( "406"
                      , Json.Decode.map
                            PatreonApi.Types.GetMember_406
                            PatreonApi.Json.decodeStatusCode406
                      )
                    , ( "410"
                      , Json.Decode.map
                            PatreonApi.Types.GetMember_410
                            PatreonApi.Json.decodeStatusCode410
                      )
                    , ( "429"
                      , Json.Decode.map
                            PatreonApi.Types.GetMember_429
                            PatreonApi.Json.decodeStatusCode429
                      )
                    , ( "500"
                      , Json.Decode.map
                            PatreonApi.Types.GetMember_500
                            PatreonApi.Json.decodeStatusCode500
                      )
                    , ( "503"
                      , Json.Decode.map
                            PatreonApi.Types.GetMember_503
                            PatreonApi.Json.decodeStatusCode503
                      )
                    ]
                )
                PatreonApi.Json.decodeMemberResponse
        , body = Http.emptyBody
        , timeout = Nothing
        }


{-| Get post
-}
getPost :
    { authorization : { oauth2 : String }
    , toMsg :
        Result (OpenApi.Common.Error PatreonApi.Types.GetPost_Error String) PatreonApi.Types.PostResponse
        -> msg
    , params :
        { include : Maybe (List String)
        , fields :
            Maybe
                { campaign : Maybe (List String)
                , post : Maybe (List String)
                , user : Maybe (List String)
                }
        , id : String
        , user_Agent : String
        }
    }
    -> Cmd msg
getPost config =
    Http.request
        { url =
            Url.Builder.crossOrigin
                "https://patreon.com"
                [ "api", "oauth", "v2", "posts", config.params.id ]
                (List.filterMap
                    Basics.identity
                    [ Maybe.map
                        (Url.Builder.string "include")
                        (Maybe.andThen
                            (\andThenUnpack ->
                                if List.isEmpty andThenUnpack then
                                    Nothing

                                else
                                    Just (String.join "," andThenUnpack)
                            )
                            config.params.include
                        )
                    , Maybe.map
                        (Url.Builder.string "fields.campaign")
                        (Maybe.andThen
                            (\andThenUnpack ->
                                if List.isEmpty andThenUnpack then
                                    Nothing

                                else
                                    Just (String.join "," andThenUnpack)
                            )
                            (Maybe.andThen .campaign config.params.fields)
                        )
                    , Maybe.map
                        (Url.Builder.string "fields.post")
                        (Maybe.andThen
                            (\andThenUnpack ->
                                if List.isEmpty andThenUnpack then
                                    Nothing

                                else
                                    Just (String.join "," andThenUnpack)
                            )
                            (Maybe.andThen .post config.params.fields)
                        )
                    , Maybe.map
                        (Url.Builder.string "fields.user")
                        (Maybe.andThen
                            (\andThenUnpack ->
                                if List.isEmpty andThenUnpack then
                                    Nothing

                                else
                                    Just (String.join "," andThenUnpack)
                            )
                            (Maybe.andThen .user config.params.fields)
                        )
                    ]
                )
        , method = "GET"
        , headers =
            [ Http.header
                "authorization"
                ("Bearer " ++ config.authorization.oauth2)
            , Http.header "User-Agent" config.params.user_Agent
            ]
        , expect =
            OpenApi.Common.expectJsonCustom
                config.toMsg
                (Dict.fromList
                    [ ( "400"
                      , Json.Decode.map
                            PatreonApi.Types.GetPost_400
                            PatreonApi.Json.decodeStatusCode400
                      )
                    , ( "401"
                      , Json.Decode.map
                            PatreonApi.Types.GetPost_401
                            PatreonApi.Json.decodeStatusCode401
                      )
                    , ( "403"
                      , Json.Decode.map
                            PatreonApi.Types.GetPost_403
                            PatreonApi.Json.decodeStatusCode403
                      )
                    , ( "404"
                      , Json.Decode.map
                            PatreonApi.Types.GetPost_404
                            PatreonApi.Json.decodeStatusCode404
                      )
                    , ( "405"
                      , Json.Decode.map
                            PatreonApi.Types.GetPost_405
                            PatreonApi.Json.decodeStatusCode405
                      )
                    , ( "406"
                      , Json.Decode.map
                            PatreonApi.Types.GetPost_406
                            PatreonApi.Json.decodeStatusCode406
                      )
                    , ( "410"
                      , Json.Decode.map
                            PatreonApi.Types.GetPost_410
                            PatreonApi.Json.decodeStatusCode410
                      )
                    , ( "429"
                      , Json.Decode.map
                            PatreonApi.Types.GetPost_429
                            PatreonApi.Json.decodeStatusCode429
                      )
                    , ( "500"
                      , Json.Decode.map
                            PatreonApi.Types.GetPost_500
                            PatreonApi.Json.decodeStatusCode500
                      )
                    , ( "503"
                      , Json.Decode.map
                            PatreonApi.Types.GetPost_503
                            PatreonApi.Json.decodeStatusCode503
                      )
                    ]
                )
                PatreonApi.Json.decodePostResponse
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        }


{-| Get post
-}
getPostTask :
    { authorization : { oauth2 : String }
    , params :
        { include : Maybe (List String)
        , fields :
            Maybe
                { campaign : Maybe (List String)
                , post : Maybe (List String)
                , user : Maybe (List String)
                }
        , id : String
        , user_Agent : String
        }
    }
    -> Task.Task (OpenApi.Common.Error PatreonApi.Types.GetPost_Error String) PatreonApi.Types.PostResponse
getPostTask config =
    Http.task
        { url =
            Url.Builder.crossOrigin
                "https://patreon.com"
                [ "api", "oauth", "v2", "posts", config.params.id ]
                (List.filterMap
                    Basics.identity
                    [ Maybe.map
                        (Url.Builder.string "include")
                        (Maybe.andThen
                            (\andThenUnpack ->
                                if List.isEmpty andThenUnpack then
                                    Nothing

                                else
                                    Just (String.join "," andThenUnpack)
                            )
                            config.params.include
                        )
                    , Maybe.map
                        (Url.Builder.string "fields.campaign")
                        (Maybe.andThen
                            (\andThenUnpack ->
                                if List.isEmpty andThenUnpack then
                                    Nothing

                                else
                                    Just (String.join "," andThenUnpack)
                            )
                            (Maybe.andThen .campaign config.params.fields)
                        )
                    , Maybe.map
                        (Url.Builder.string "fields.post")
                        (Maybe.andThen
                            (\andThenUnpack ->
                                if List.isEmpty andThenUnpack then
                                    Nothing

                                else
                                    Just (String.join "," andThenUnpack)
                            )
                            (Maybe.andThen .post config.params.fields)
                        )
                    , Maybe.map
                        (Url.Builder.string "fields.user")
                        (Maybe.andThen
                            (\andThenUnpack ->
                                if List.isEmpty andThenUnpack then
                                    Nothing

                                else
                                    Just (String.join "," andThenUnpack)
                            )
                            (Maybe.andThen .user config.params.fields)
                        )
                    ]
                )
        , method = "GET"
        , headers =
            [ Http.header
                "authorization"
                ("Bearer " ++ config.authorization.oauth2)
            , Http.header "User-Agent" config.params.user_Agent
            ]
        , resolver =
            OpenApi.Common.jsonResolverCustom
                (Dict.fromList
                    [ ( "400"
                      , Json.Decode.map
                            PatreonApi.Types.GetPost_400
                            PatreonApi.Json.decodeStatusCode400
                      )
                    , ( "401"
                      , Json.Decode.map
                            PatreonApi.Types.GetPost_401
                            PatreonApi.Json.decodeStatusCode401
                      )
                    , ( "403"
                      , Json.Decode.map
                            PatreonApi.Types.GetPost_403
                            PatreonApi.Json.decodeStatusCode403
                      )
                    , ( "404"
                      , Json.Decode.map
                            PatreonApi.Types.GetPost_404
                            PatreonApi.Json.decodeStatusCode404
                      )
                    , ( "405"
                      , Json.Decode.map
                            PatreonApi.Types.GetPost_405
                            PatreonApi.Json.decodeStatusCode405
                      )
                    , ( "406"
                      , Json.Decode.map
                            PatreonApi.Types.GetPost_406
                            PatreonApi.Json.decodeStatusCode406
                      )
                    , ( "410"
                      , Json.Decode.map
                            PatreonApi.Types.GetPost_410
                            PatreonApi.Json.decodeStatusCode410
                      )
                    , ( "429"
                      , Json.Decode.map
                            PatreonApi.Types.GetPost_429
                            PatreonApi.Json.decodeStatusCode429
                      )
                    , ( "500"
                      , Json.Decode.map
                            PatreonApi.Types.GetPost_500
                            PatreonApi.Json.decodeStatusCode500
                      )
                    , ( "503"
                      , Json.Decode.map
                            PatreonApi.Types.GetPost_503
                            PatreonApi.Json.decodeStatusCode503
                      )
                    ]
                )
                PatreonApi.Json.decodePostResponse
        , body = Http.emptyBody
        , timeout = Nothing
        }


{-| Get webhooks
-}
getWebhooks :
    { authorization : { oauth2 : String }
    , toMsg :
        Result (OpenApi.Common.Error PatreonApi.Types.GetWebhooks_Error String) PatreonApi.Types.WebhooksResponse
        -> msg
    , params :
        { include : Maybe (List String)
        , fields :
            Maybe
                { campaign : Maybe (List String)
                , client : Maybe (List String)
                , webhook : Maybe (List String)
                }
        , sort : Maybe String
        , page : Maybe { count : Maybe Float, cursor : Maybe String }
        , user_Agent : String
        }
    }
    -> Cmd msg
getWebhooks config =
    Http.request
        { url =
            Url.Builder.crossOrigin
                "https://patreon.com"
                [ "api", "oauth", "v2", "webhooks" ]
                (List.filterMap
                    Basics.identity
                    [ Maybe.map
                        (Url.Builder.string "include")
                        (Maybe.andThen
                            (\andThenUnpack ->
                                if List.isEmpty andThenUnpack then
                                    Nothing

                                else
                                    Just (String.join "," andThenUnpack)
                            )
                            config.params.include
                        )
                    , Maybe.map
                        (Url.Builder.string "fields.campaign")
                        (Maybe.andThen
                            (\andThenUnpack ->
                                if List.isEmpty andThenUnpack then
                                    Nothing

                                else
                                    Just (String.join "," andThenUnpack)
                            )
                            (Maybe.andThen .campaign config.params.fields)
                        )
                    , Maybe.map
                        (Url.Builder.string "fields.client")
                        (Maybe.andThen
                            (\andThenUnpack ->
                                if List.isEmpty andThenUnpack then
                                    Nothing

                                else
                                    Just (String.join "," andThenUnpack)
                            )
                            (Maybe.andThen .client config.params.fields)
                        )
                    , Maybe.map
                        (Url.Builder.string "fields.webhook")
                        (Maybe.andThen
                            (\andThenUnpack ->
                                if List.isEmpty andThenUnpack then
                                    Nothing

                                else
                                    Just (String.join "," andThenUnpack)
                            )
                            (Maybe.andThen .webhook config.params.fields)
                        )
                    , Maybe.map (Url.Builder.string "sort") config.params.sort
                    , Maybe.map
                        (Url.Builder.string "page.count")
                        (Maybe.map
                            String.fromFloat
                            (Maybe.andThen .count config.params.page)
                        )
                    , Maybe.map
                        (Url.Builder.string "page.cursor")
                        (Maybe.andThen .cursor config.params.page)
                    ]
                )
        , method = "GET"
        , headers =
            [ Http.header
                "authorization"
                ("Bearer " ++ config.authorization.oauth2)
            , Http.header "User-Agent" config.params.user_Agent
            ]
        , expect =
            OpenApi.Common.expectJsonCustom
                config.toMsg
                (Dict.fromList
                    [ ( "400"
                      , Json.Decode.map
                            PatreonApi.Types.GetWebhooks_400
                            PatreonApi.Json.decodeStatusCode400
                      )
                    , ( "401"
                      , Json.Decode.map
                            PatreonApi.Types.GetWebhooks_401
                            PatreonApi.Json.decodeStatusCode401
                      )
                    , ( "403"
                      , Json.Decode.map
                            PatreonApi.Types.GetWebhooks_403
                            PatreonApi.Json.decodeStatusCode403
                      )
                    , ( "404"
                      , Json.Decode.map
                            PatreonApi.Types.GetWebhooks_404
                            PatreonApi.Json.decodeStatusCode404
                      )
                    , ( "405"
                      , Json.Decode.map
                            PatreonApi.Types.GetWebhooks_405
                            PatreonApi.Json.decodeStatusCode405
                      )
                    , ( "406"
                      , Json.Decode.map
                            PatreonApi.Types.GetWebhooks_406
                            PatreonApi.Json.decodeStatusCode406
                      )
                    , ( "410"
                      , Json.Decode.map
                            PatreonApi.Types.GetWebhooks_410
                            PatreonApi.Json.decodeStatusCode410
                      )
                    , ( "429"
                      , Json.Decode.map
                            PatreonApi.Types.GetWebhooks_429
                            PatreonApi.Json.decodeStatusCode429
                      )
                    , ( "500"
                      , Json.Decode.map
                            PatreonApi.Types.GetWebhooks_500
                            PatreonApi.Json.decodeStatusCode500
                      )
                    , ( "503"
                      , Json.Decode.map
                            PatreonApi.Types.GetWebhooks_503
                            PatreonApi.Json.decodeStatusCode503
                      )
                    ]
                )
                PatreonApi.Json.decodeWebhooksResponse
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        }


{-| Get webhooks
-}
getWebhooksTask :
    { authorization : { oauth2 : String }
    , params :
        { include : Maybe (List String)
        , fields :
            Maybe
                { campaign : Maybe (List String)
                , client : Maybe (List String)
                , webhook : Maybe (List String)
                }
        , sort : Maybe String
        , page : Maybe { count : Maybe Float, cursor : Maybe String }
        , user_Agent : String
        }
    }
    -> Task.Task (OpenApi.Common.Error PatreonApi.Types.GetWebhooks_Error String) PatreonApi.Types.WebhooksResponse
getWebhooksTask config =
    Http.task
        { url =
            Url.Builder.crossOrigin
                "https://patreon.com"
                [ "api", "oauth", "v2", "webhooks" ]
                (List.filterMap
                    Basics.identity
                    [ Maybe.map
                        (Url.Builder.string "include")
                        (Maybe.andThen
                            (\andThenUnpack ->
                                if List.isEmpty andThenUnpack then
                                    Nothing

                                else
                                    Just (String.join "," andThenUnpack)
                            )
                            config.params.include
                        )
                    , Maybe.map
                        (Url.Builder.string "fields.campaign")
                        (Maybe.andThen
                            (\andThenUnpack ->
                                if List.isEmpty andThenUnpack then
                                    Nothing

                                else
                                    Just (String.join "," andThenUnpack)
                            )
                            (Maybe.andThen .campaign config.params.fields)
                        )
                    , Maybe.map
                        (Url.Builder.string "fields.client")
                        (Maybe.andThen
                            (\andThenUnpack ->
                                if List.isEmpty andThenUnpack then
                                    Nothing

                                else
                                    Just (String.join "," andThenUnpack)
                            )
                            (Maybe.andThen .client config.params.fields)
                        )
                    , Maybe.map
                        (Url.Builder.string "fields.webhook")
                        (Maybe.andThen
                            (\andThenUnpack ->
                                if List.isEmpty andThenUnpack then
                                    Nothing

                                else
                                    Just (String.join "," andThenUnpack)
                            )
                            (Maybe.andThen .webhook config.params.fields)
                        )
                    , Maybe.map (Url.Builder.string "sort") config.params.sort
                    , Maybe.map
                        (Url.Builder.string "page.count")
                        (Maybe.map
                            String.fromFloat
                            (Maybe.andThen .count config.params.page)
                        )
                    , Maybe.map
                        (Url.Builder.string "page.cursor")
                        (Maybe.andThen .cursor config.params.page)
                    ]
                )
        , method = "GET"
        , headers =
            [ Http.header
                "authorization"
                ("Bearer " ++ config.authorization.oauth2)
            , Http.header "User-Agent" config.params.user_Agent
            ]
        , resolver =
            OpenApi.Common.jsonResolverCustom
                (Dict.fromList
                    [ ( "400"
                      , Json.Decode.map
                            PatreonApi.Types.GetWebhooks_400
                            PatreonApi.Json.decodeStatusCode400
                      )
                    , ( "401"
                      , Json.Decode.map
                            PatreonApi.Types.GetWebhooks_401
                            PatreonApi.Json.decodeStatusCode401
                      )
                    , ( "403"
                      , Json.Decode.map
                            PatreonApi.Types.GetWebhooks_403
                            PatreonApi.Json.decodeStatusCode403
                      )
                    , ( "404"
                      , Json.Decode.map
                            PatreonApi.Types.GetWebhooks_404
                            PatreonApi.Json.decodeStatusCode404
                      )
                    , ( "405"
                      , Json.Decode.map
                            PatreonApi.Types.GetWebhooks_405
                            PatreonApi.Json.decodeStatusCode405
                      )
                    , ( "406"
                      , Json.Decode.map
                            PatreonApi.Types.GetWebhooks_406
                            PatreonApi.Json.decodeStatusCode406
                      )
                    , ( "410"
                      , Json.Decode.map
                            PatreonApi.Types.GetWebhooks_410
                            PatreonApi.Json.decodeStatusCode410
                      )
                    , ( "429"
                      , Json.Decode.map
                            PatreonApi.Types.GetWebhooks_429
                            PatreonApi.Json.decodeStatusCode429
                      )
                    , ( "500"
                      , Json.Decode.map
                            PatreonApi.Types.GetWebhooks_500
                            PatreonApi.Json.decodeStatusCode500
                      )
                    , ( "503"
                      , Json.Decode.map
                            PatreonApi.Types.GetWebhooks_503
                            PatreonApi.Json.decodeStatusCode503
                      )
                    ]
                )
                PatreonApi.Json.decodeWebhooksResponse
        , body = Http.emptyBody
        , timeout = Nothing
        }
