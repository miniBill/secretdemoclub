module PatreonApi.Json exposing
    ( encodeAddress, encodeBenefit, encodeCampaign, encodeCampaignResponse, encodeCampaignsResponse
    , encodeClient, encodeDeliverable, encodeGoal, encodeJSONAPIError, encodeJSONAPILinksRelated
    , encodeJSONAPIResource, encodeJSONAPIResponseLinks, encodeJSONAPIResponseMeta, encodeMedia, encodeMember
    , encodeMemberResponse, encodeMembersResponse, encodePledgeEvent, encodePost, encodePostResponse
    , encodePostsResponse, encodeStatusCode400, encodeStatusCode401, encodeStatusCode403, encodeStatusCode404
    , encodeStatusCode405, encodeStatusCode406, encodeStatusCode410, encodeStatusCode429, encodeStatusCode500
    , encodeStatusCode503, encodeTier, encodeUser, encodeUserResponse, encodeWebhook, encodeWebhookResponse
    , encodeWebhookTrigger, encodeWebhooksResponse
    , decodeAddress, decodeBenefit, decodeCampaign, decodeCampaignResponse, decodeCampaignsResponse
    , decodeClient, decodeDeliverable, decodeGoal, decodeJSONAPIError, decodeJSONAPILinksRelated
    , decodeJSONAPIResource, decodeJSONAPIResponseLinks, decodeJSONAPIResponseMeta, decodeMedia, decodeMember
    , decodeMemberResponse, decodeMembersResponse, decodePledgeEvent, decodePost, decodePostResponse
    , decodePostsResponse, decodeStatusCode400, decodeStatusCode401, decodeStatusCode403, decodeStatusCode404
    , decodeStatusCode405, decodeStatusCode406, decodeStatusCode410, decodeStatusCode429, decodeStatusCode500
    , decodeStatusCode503, decodeTier, decodeUser, decodeUserResponse, decodeWebhook, decodeWebhookResponse
    , decodeWebhookTrigger, decodeWebhooksResponse
    )

{-|


## Encoders

@docs encodeAddress, encodeBenefit, encodeCampaign, encodeCampaignResponse, encodeCampaignsResponse
@docs encodeClient, encodeDeliverable, encodeGoal, encodeJSONAPIError, encodeJSONAPILinksRelated
@docs encodeJSONAPIResource, encodeJSONAPIResponseLinks, encodeJSONAPIResponseMeta, encodeMedia, encodeMember
@docs encodeMemberResponse, encodeMembersResponse, encodePledgeEvent, encodePost, encodePostResponse
@docs encodePostsResponse, encodeStatusCode400, encodeStatusCode401, encodeStatusCode403, encodeStatusCode404
@docs encodeStatusCode405, encodeStatusCode406, encodeStatusCode410, encodeStatusCode429, encodeStatusCode500
@docs encodeStatusCode503, encodeTier, encodeUser, encodeUserResponse, encodeWebhook, encodeWebhookResponse
@docs encodeWebhookTrigger, encodeWebhooksResponse


## Decoders

@docs decodeAddress, decodeBenefit, decodeCampaign, decodeCampaignResponse, decodeCampaignsResponse
@docs decodeClient, decodeDeliverable, decodeGoal, decodeJSONAPIError, decodeJSONAPILinksRelated
@docs decodeJSONAPIResource, decodeJSONAPIResponseLinks, decodeJSONAPIResponseMeta, decodeMedia, decodeMember
@docs decodeMemberResponse, decodeMembersResponse, decodePledgeEvent, decodePost, decodePostResponse
@docs decodePostsResponse, decodeStatusCode400, decodeStatusCode401, decodeStatusCode403, decodeStatusCode404
@docs decodeStatusCode405, decodeStatusCode406, decodeStatusCode410, decodeStatusCode429, decodeStatusCode500
@docs decodeStatusCode503, decodeTier, decodeUser, decodeUserResponse, decodeWebhook, decodeWebhookResponse
@docs decodeWebhookTrigger, decodeWebhooksResponse

-}

import Json.Decode
import Json.Encode
import OpenApi.Common
import PatreonApi.Types
import Time
import Url


decodeAddress : Json.Decode.Decoder PatreonApi.Types.Address
decodeAddress =
    Json.Decode.succeed
        (\addressee city country created_at line_1 line_2 phone_number postal_code state ->
            { addressee = addressee
            , city = city
            , country = country
            , created_at = created_at
            , line_1 = line_1
            , line_2 = line_2
            , phone_number = phone_number
            , postal_code = postal_code
            , state = state
            }
        )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "addressee"
                (Json.Decode.oneOf
                    [ Json.Decode.map
                        OpenApi.Common.Present
                        Json.Decode.string
                    , Json.Decode.null OpenApi.Common.Null
                    ]
                )
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "city"
                Json.Decode.string
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "country"
                Json.Decode.string
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "created_at"
                OpenApi.Common.decodeStringDateTime
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "line_1"
                (Json.Decode.oneOf
                    [ Json.Decode.map
                        OpenApi.Common.Present
                        Json.Decode.string
                    , Json.Decode.null
                        OpenApi.Common.Null
                    ]
                )
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "line_2"
                (Json.Decode.oneOf
                    [ Json.Decode.map
                        OpenApi.Common.Present
                        Json.Decode.string
                    , Json.Decode.null
                        OpenApi.Common.Null
                    ]
                )
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "phone_number"
                (Json.Decode.oneOf
                    [ Json.Decode.map
                        OpenApi.Common.Present
                        Json.Decode.string
                    , Json.Decode.null
                        OpenApi.Common.Null
                    ]
                )
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "postal_code"
                (Json.Decode.oneOf
                    [ Json.Decode.map
                        OpenApi.Common.Present
                        Json.Decode.string
                    , Json.Decode.null
                        OpenApi.Common.Null
                    ]
                )
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "state"
                (Json.Decode.oneOf
                    [ Json.Decode.map
                        OpenApi.Common.Present
                        Json.Decode.string
                    , Json.Decode.null
                        OpenApi.Common.Null
                    ]
                )
            )


decodeBenefit : Json.Decode.Decoder PatreonApi.Types.Benefit
decodeBenefit =
    Json.Decode.succeed
        (\app_external_id app_meta benefit_type created_at deliverables_due_today_count delivered_deliverables_count description is_deleted is_ended is_published next_deliverable_due_date not_delivered_deliverables_count rule_type tiers_count title ->
            { app_external_id = app_external_id
            , app_meta = app_meta
            , benefit_type = benefit_type
            , created_at = created_at
            , deliverables_due_today_count = deliverables_due_today_count
            , delivered_deliverables_count = delivered_deliverables_count
            , description = description
            , is_deleted = is_deleted
            , is_ended = is_ended
            , is_published = is_published
            , next_deliverable_due_date = next_deliverable_due_date
            , not_delivered_deliverables_count =
                not_delivered_deliverables_count
            , rule_type = rule_type
            , tiers_count = tiers_count
            , title = title
            }
        )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "app_external_id"
                (Json.Decode.oneOf
                    [ Json.Decode.map
                        OpenApi.Common.Present
                        Json.Decode.string
                    , Json.Decode.null OpenApi.Common.Null
                    ]
                )
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "app_meta"
                (Json.Decode.oneOf
                    [ Json.Decode.map
                        OpenApi.Common.Present
                        (Json.Decode.succeed {})
                    , Json.Decode.null
                        OpenApi.Common.Null
                    ]
                )
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "benefit_type"
                (Json.Decode.oneOf
                    [ Json.Decode.map
                        OpenApi.Common.Present
                        Json.Decode.string
                    , Json.Decode.null
                        OpenApi.Common.Null
                    ]
                )
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "created_at"
                OpenApi.Common.decodeStringDateTime
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "deliverables_due_today_count"
                Json.Decode.float
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "delivered_deliverables_count"
                Json.Decode.float
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "description"
                (Json.Decode.oneOf
                    [ Json.Decode.map
                        OpenApi.Common.Present
                        Json.Decode.string
                    , Json.Decode.null
                        OpenApi.Common.Null
                    ]
                )
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "is_deleted"
                Json.Decode.bool
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "is_ended"
                Json.Decode.bool
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "is_published"
                Json.Decode.bool
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "next_deliverable_due_date"
                (Json.Decode.oneOf
                    [ Json.Decode.map
                        OpenApi.Common.Present
                        OpenApi.Common.decodeStringDateTime
                    , Json.Decode.null
                        OpenApi.Common.Null
                    ]
                )
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "not_delivered_deliverables_count"
                Json.Decode.float
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "rule_type"
                (Json.Decode.oneOf
                    [ Json.Decode.map
                        OpenApi.Common.Present
                        Json.Decode.string
                    , Json.Decode.null
                        OpenApi.Common.Null
                    ]
                )
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "tiers_count"
                Json.Decode.float
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "title"
                Json.Decode.string
            )


decodeCampaign : Json.Decode.Decoder PatreonApi.Types.Campaign
decodeCampaign =
    Json.Decode.succeed
        (\created_at creation_name discord_server_id google_analytics_id has_rss has_sent_rss_notify image_small_url image_url is_charged_immediately is_monthly is_nsfw main_video_embed main_video_url one_liner patron_count pay_per_name pledge_url published_at rss_artwork_url rss_feed_title show_earnings summary thanks_embed thanks_msg thanks_video_url url vanity ->
            { created_at = created_at
            , creation_name = creation_name
            , discord_server_id = discord_server_id
            , google_analytics_id = google_analytics_id
            , has_rss = has_rss
            , has_sent_rss_notify = has_sent_rss_notify
            , image_small_url = image_small_url
            , image_url = image_url
            , is_charged_immediately = is_charged_immediately
            , is_monthly = is_monthly
            , is_nsfw = is_nsfw
            , main_video_embed = main_video_embed
            , main_video_url = main_video_url
            , one_liner = one_liner
            , patron_count = patron_count
            , pay_per_name = pay_per_name
            , pledge_url = pledge_url
            , published_at = published_at
            , rss_artwork_url = rss_artwork_url
            , rss_feed_title = rss_feed_title
            , show_earnings = show_earnings
            , summary = summary
            , thanks_embed = thanks_embed
            , thanks_msg = thanks_msg
            , thanks_video_url = thanks_video_url
            , url = url
            , vanity = vanity
            }
        )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "created_at"
                OpenApi.Common.decodeStringDateTime
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "creation_name"
                (Json.Decode.oneOf
                    [ Json.Decode.map
                        OpenApi.Common.Present
                        Json.Decode.string
                    , Json.Decode.null
                        OpenApi.Common.Null
                    ]
                )
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "discord_server_id"
                (Json.Decode.oneOf
                    [ Json.Decode.map
                        OpenApi.Common.Present
                        Json.Decode.string
                    , Json.Decode.null
                        OpenApi.Common.Null
                    ]
                )
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "google_analytics_id"
                (Json.Decode.oneOf
                    [ Json.Decode.map
                        OpenApi.Common.Present
                        Json.Decode.string
                    , Json.Decode.null
                        OpenApi.Common.Null
                    ]
                )
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "has_rss"
                Json.Decode.bool
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "has_sent_rss_notify"
                Json.Decode.bool
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "image_small_url"
                OpenApi.Common.decodeStringUri
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "image_url"
                OpenApi.Common.decodeStringUri
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "is_charged_immediately"
                Json.Decode.bool
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "is_monthly"
                Json.Decode.bool
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "is_nsfw"
                Json.Decode.bool
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "main_video_embed"
                (Json.Decode.oneOf
                    [ Json.Decode.map
                        OpenApi.Common.Present
                        Json.Decode.string
                    , Json.Decode.null
                        OpenApi.Common.Null
                    ]
                )
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "main_video_url"
                (Json.Decode.oneOf
                    [ Json.Decode.map
                        OpenApi.Common.Present
                        OpenApi.Common.decodeStringUri
                    , Json.Decode.null
                        OpenApi.Common.Null
                    ]
                )
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "one_liner"
                (Json.Decode.oneOf
                    [ Json.Decode.map
                        OpenApi.Common.Present
                        Json.Decode.string
                    , Json.Decode.null
                        OpenApi.Common.Null
                    ]
                )
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "patron_count"
                Json.Decode.float
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "pay_per_name"
                (Json.Decode.oneOf
                    [ Json.Decode.map
                        OpenApi.Common.Present
                        Json.Decode.string
                    , Json.Decode.null
                        OpenApi.Common.Null
                    ]
                )
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "pledge_url"
                Json.Decode.string
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "published_at"
                (Json.Decode.oneOf
                    [ Json.Decode.map
                        OpenApi.Common.Present
                        OpenApi.Common.decodeStringDateTime
                    , Json.Decode.null
                        OpenApi.Common.Null
                    ]
                )
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "rss_artwork_url"
                (Json.Decode.oneOf
                    [ Json.Decode.map
                        OpenApi.Common.Present
                        OpenApi.Common.decodeStringUri
                    , Json.Decode.null
                        OpenApi.Common.Null
                    ]
                )
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "rss_feed_title"
                (Json.Decode.oneOf
                    [ Json.Decode.map
                        OpenApi.Common.Present
                        Json.Decode.string
                    , Json.Decode.null
                        OpenApi.Common.Null
                    ]
                )
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "show_earnings"
                Json.Decode.bool
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "summary"
                (Json.Decode.oneOf
                    [ Json.Decode.map
                        OpenApi.Common.Present
                        Json.Decode.string
                    , Json.Decode.null
                        OpenApi.Common.Null
                    ]
                )
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "thanks_embed"
                (Json.Decode.oneOf
                    [ Json.Decode.map
                        OpenApi.Common.Present
                        Json.Decode.string
                    , Json.Decode.null
                        OpenApi.Common.Null
                    ]
                )
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "thanks_msg"
                (Json.Decode.oneOf
                    [ Json.Decode.map
                        OpenApi.Common.Present
                        Json.Decode.string
                    , Json.Decode.null
                        OpenApi.Common.Null
                    ]
                )
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "thanks_video_url"
                (Json.Decode.oneOf
                    [ Json.Decode.map
                        OpenApi.Common.Present
                        OpenApi.Common.decodeStringUri
                    , Json.Decode.null
                        OpenApi.Common.Null
                    ]
                )
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "url"
                OpenApi.Common.decodeStringUri
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "vanity"
                (Json.Decode.oneOf
                    [ Json.Decode.map
                        OpenApi.Common.Present
                        Json.Decode.string
                    , Json.Decode.null
                        OpenApi.Common.Null
                    ]
                )
            )


decodeCampaignResponse : Json.Decode.Decoder PatreonApi.Types.CampaignResponse
decodeCampaignResponse =
    Json.Decode.succeed
        (\data included links ->
            { data = data, included = included, links = links }
        )
        |> OpenApi.Common.jsonDecodeAndMap
            (Json.Decode.field
                "data"
                (Json.Decode.succeed
                    (\attributes id relationships type_ ->
                        { attributes = attributes
                        , id = id
                        , relationships = relationships
                        , type_ = type_
                        }
                    )
                    |> OpenApi.Common.jsonDecodeAndMap
                        (OpenApi.Common.decodeOptionalField
                            "attributes"
                            decodeCampaign
                        )
                    |> OpenApi.Common.jsonDecodeAndMap
                        (Json.Decode.field
                            "id"
                            Json.Decode.string
                        )
                    |> OpenApi.Common.jsonDecodeAndMap
                        (OpenApi.Common.decodeOptionalField
                            "relationships"
                            Json.Decode.value
                        )
                    |> OpenApi.Common.jsonDecodeAndMap
                        (Json.Decode.field
                            "type"
                            Json.Decode.string
                        )
                )
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "included"
                (Json.Decode.list Json.Decode.value)
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (Json.Decode.field
                "links"
                decodeJSONAPIResponseLinks
            )


decodeCampaignsResponse : Json.Decode.Decoder PatreonApi.Types.CampaignsResponse
decodeCampaignsResponse =
    Json.Decode.succeed
        (\data included meta ->
            { data = data, included = included, meta = meta }
        )
        |> OpenApi.Common.jsonDecodeAndMap
            (Json.Decode.field
                "data"
                (Json.Decode.list
                    (Json.Decode.succeed
                        (\attributes id relationships type_ ->
                            { attributes =
                                attributes
                            , id = id
                            , relationships =
                                relationships
                            , type_ = type_
                            }
                        )
                        |> OpenApi.Common.jsonDecodeAndMap
                            (OpenApi.Common.decodeOptionalField
                                "attributes"
                                decodeCampaign
                            )
                        |> OpenApi.Common.jsonDecodeAndMap
                            (Json.Decode.field
                                "id"
                                Json.Decode.string
                            )
                        |> OpenApi.Common.jsonDecodeAndMap
                            (OpenApi.Common.decodeOptionalField
                                "relationships"
                                Json.Decode.value
                            )
                        |> OpenApi.Common.jsonDecodeAndMap
                            (Json.Decode.field
                                "type"
                                Json.Decode.string
                            )
                    )
                )
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "included"
                (Json.Decode.list Json.Decode.value)
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (Json.Decode.field
                "meta"
                decodeJSONAPIResponseMeta
            )


decodeClient : Json.Decode.Decoder PatreonApi.Types.Client
decodeClient =
    Json.Decode.succeed
        (\author_name category client_secret default_scopes description domain icon_url name privacy_policy_url redirect_uris tos_url version ->
            { author_name = author_name
            , category = category
            , client_secret = client_secret
            , default_scopes = default_scopes
            , description = description
            , domain = domain
            , icon_url = icon_url
            , name = name
            , privacy_policy_url = privacy_policy_url
            , redirect_uris = redirect_uris
            , tos_url = tos_url
            , version = version
            }
        )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "author_name"
                (Json.Decode.oneOf
                    [ Json.Decode.map
                        OpenApi.Common.Present
                        Json.Decode.string
                    , Json.Decode.null OpenApi.Common.Null
                    ]
                )
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "category"
                Json.Decode.string
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "client_secret"
                Json.Decode.string
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "default_scopes"
                Json.Decode.string
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "description"
                Json.Decode.string
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "domain"
                (Json.Decode.oneOf
                    [ Json.Decode.map
                        OpenApi.Common.Present
                        OpenApi.Common.decodeStringUri
                    , Json.Decode.null
                        OpenApi.Common.Null
                    ]
                )
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "icon_url"
                (Json.Decode.oneOf
                    [ Json.Decode.map
                        OpenApi.Common.Present
                        OpenApi.Common.decodeStringUri
                    , Json.Decode.null
                        OpenApi.Common.Null
                    ]
                )
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "name"
                Json.Decode.string
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "privacy_policy_url"
                (Json.Decode.oneOf
                    [ Json.Decode.map
                        OpenApi.Common.Present
                        OpenApi.Common.decodeStringUri
                    , Json.Decode.null
                        OpenApi.Common.Null
                    ]
                )
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "redirect_uris"
                Json.Decode.string
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "tos_url"
                OpenApi.Common.decodeStringUri
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "version"
                Json.Decode.float
            )


decodeDeliverable : Json.Decode.Decoder PatreonApi.Types.Deliverable
decodeDeliverable =
    Json.Decode.succeed
        (\completed_at delivery_status due_at ->
            { completed_at = completed_at
            , delivery_status = delivery_status
            , due_at = due_at
            }
        )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "completed_at"
                (Json.Decode.oneOf
                    [ Json.Decode.map
                        OpenApi.Common.Present
                        OpenApi.Common.decodeStringDateTime
                    , Json.Decode.null OpenApi.Common.Null
                    ]
                )
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "delivery_status"
                Json.Decode.string
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "due_at"
                OpenApi.Common.decodeStringDateTime
            )


decodeGoal : Json.Decode.Decoder PatreonApi.Types.Goal
decodeGoal =
    Json.Decode.succeed {}


decodeJSONAPIError : Json.Decode.Decoder PatreonApi.Types.JSONAPIError
decodeJSONAPIError =
    Json.Decode.succeed
        (\code code_name detail id status title ->
            { code = code
            , code_name = code_name
            , detail = detail
            , id = id
            , status = status
            , title = title
            }
        )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "code"
                Json.Decode.float
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "code_name"
                Json.Decode.string
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "detail"
                Json.Decode.string
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "id"
                Json.Decode.string
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "status"
                Json.Decode.string
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "title"
                Json.Decode.string
            )


decodeJSONAPILinksRelated : Json.Decode.Decoder PatreonApi.Types.JSONAPILinksRelated
decodeJSONAPILinksRelated =
    Json.Decode.value


decodeJSONAPIResource : Json.Decode.Decoder PatreonApi.Types.JSONAPIResource
decodeJSONAPIResource =
    Json.Decode.succeed
        (\id type_ -> { id = id, type_ = type_ })
        |> OpenApi.Common.jsonDecodeAndMap
            (Json.Decode.field "id" Json.Decode.string)
        |> OpenApi.Common.jsonDecodeAndMap
            (Json.Decode.field "type" Json.Decode.string)


decodeJSONAPIResponseLinks : Json.Decode.Decoder PatreonApi.Types.JSONAPIResponseLinks
decodeJSONAPIResponseLinks =
    Json.Decode.value


decodeJSONAPIResponseMeta : Json.Decode.Decoder PatreonApi.Types.JSONAPIResponseMeta
decodeJSONAPIResponseMeta =
    Json.Decode.value


decodeMedia : Json.Decode.Decoder PatreonApi.Types.Media
decodeMedia =
    Json.Decode.succeed
        (\created_at download_url file_name image_urls metadata mimetype owner_id owner_relationship owner_type size_bytes state upload_expires_at upload_parameters upload_url ->
            { created_at = created_at
            , download_url = download_url
            , file_name = file_name
            , image_urls = image_urls
            , metadata = metadata
            , mimetype = mimetype
            , owner_id = owner_id
            , owner_relationship = owner_relationship
            , owner_type = owner_type
            , size_bytes = size_bytes
            , state = state
            , upload_expires_at = upload_expires_at
            , upload_parameters = upload_parameters
            , upload_url = upload_url
            }
        )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "created_at"
                OpenApi.Common.decodeStringDateTime
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "download_url"
                OpenApi.Common.decodeStringUri
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "file_name"
                Json.Decode.string
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "image_urls"
                (Json.Decode.succeed
                    {}
                )
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "metadata"
                (Json.Decode.oneOf
                    [ Json.Decode.map
                        OpenApi.Common.Present
                        (Json.Decode.succeed
                            {}
                        )
                    , Json.Decode.null
                        OpenApi.Common.Null
                    ]
                )
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "mimetype"
                Json.Decode.string
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "owner_id"
                Json.Decode.string
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "owner_relationship"
                Json.Decode.string
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "owner_type"
                Json.Decode.string
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "size_bytes"
                Json.Decode.float
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "state"
                Json.Decode.string
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "upload_expires_at"
                OpenApi.Common.decodeStringDateTime
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "upload_parameters"
                (Json.Decode.succeed
                    {}
                )
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "upload_url"
                OpenApi.Common.decodeStringUri
            )


decodeMember : Json.Decode.Decoder PatreonApi.Types.Member
decodeMember =
    Json.Decode.succeed
        (\campaign_lifetime_support_cents currently_entitled_amount_cents email full_name is_follower is_free_trial is_gifted last_charge_date last_charge_status lifetime_support_cents next_charge_date note patron_status pledge_cadence pledge_relationship_start will_pay_amount_cents ->
            { campaign_lifetime_support_cents = campaign_lifetime_support_cents
            , currently_entitled_amount_cents = currently_entitled_amount_cents
            , email = email
            , full_name = full_name
            , is_follower = is_follower
            , is_free_trial = is_free_trial
            , is_gifted = is_gifted
            , last_charge_date = last_charge_date
            , last_charge_status = last_charge_status
            , lifetime_support_cents = lifetime_support_cents
            , next_charge_date = next_charge_date
            , note = note
            , patron_status = patron_status
            , pledge_cadence = pledge_cadence
            , pledge_relationship_start = pledge_relationship_start
            , will_pay_amount_cents = will_pay_amount_cents
            }
        )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "campaign_lifetime_support_cents"
                Json.Decode.float
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "currently_entitled_amount_cents"
                Json.Decode.float
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "email"
                Json.Decode.string
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "full_name"
                Json.Decode.string
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "is_follower"
                Json.Decode.bool
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "is_free_trial"
                Json.Decode.bool
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "is_gifted"
                Json.Decode.bool
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "last_charge_date"
                (Json.Decode.oneOf
                    [ Json.Decode.map
                        OpenApi.Common.Present
                        OpenApi.Common.decodeStringDateTime
                    , Json.Decode.null
                        OpenApi.Common.Null
                    ]
                )
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "last_charge_status"
                (Json.Decode.oneOf
                    [ Json.Decode.map
                        OpenApi.Common.Present
                        Json.Decode.string
                    , Json.Decode.null
                        OpenApi.Common.Null
                    ]
                )
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "lifetime_support_cents"
                Json.Decode.float
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "next_charge_date"
                (Json.Decode.oneOf
                    [ Json.Decode.map
                        OpenApi.Common.Present
                        OpenApi.Common.decodeStringDateTime
                    , Json.Decode.null
                        OpenApi.Common.Null
                    ]
                )
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "note"
                Json.Decode.string
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "patron_status"
                (Json.Decode.oneOf
                    [ Json.Decode.map
                        OpenApi.Common.Present
                        Json.Decode.string
                    , Json.Decode.null
                        OpenApi.Common.Null
                    ]
                )
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "pledge_cadence"
                (Json.Decode.oneOf
                    [ Json.Decode.map
                        OpenApi.Common.Present
                        Json.Decode.float
                    , Json.Decode.null
                        OpenApi.Common.Null
                    ]
                )
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "pledge_relationship_start"
                (Json.Decode.oneOf
                    [ Json.Decode.map
                        OpenApi.Common.Present
                        OpenApi.Common.decodeStringDateTime
                    , Json.Decode.null
                        OpenApi.Common.Null
                    ]
                )
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "will_pay_amount_cents"
                Json.Decode.float
            )


decodeMemberResponse : Json.Decode.Decoder PatreonApi.Types.MemberResponse
decodeMemberResponse =
    Json.Decode.succeed
        (\data included links ->
            { data = data, included = included, links = links }
        )
        |> OpenApi.Common.jsonDecodeAndMap
            (Json.Decode.field
                "data"
                (Json.Decode.succeed
                    (\attributes id relationships type_ ->
                        { attributes = attributes
                        , id = id
                        , relationships = relationships
                        , type_ = type_
                        }
                    )
                    |> OpenApi.Common.jsonDecodeAndMap
                        (OpenApi.Common.decodeOptionalField
                            "attributes"
                            decodeMember
                        )
                    |> OpenApi.Common.jsonDecodeAndMap
                        (Json.Decode.field
                            "id"
                            Json.Decode.string
                        )
                    |> OpenApi.Common.jsonDecodeAndMap
                        (OpenApi.Common.decodeOptionalField
                            "relationships"
                            Json.Decode.value
                        )
                    |> OpenApi.Common.jsonDecodeAndMap
                        (Json.Decode.field
                            "type"
                            Json.Decode.string
                        )
                )
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "included"
                (Json.Decode.list Json.Decode.value)
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (Json.Decode.field
                "links"
                decodeJSONAPIResponseLinks
            )


decodeMembersResponse : Json.Decode.Decoder PatreonApi.Types.MembersResponse
decodeMembersResponse =
    Json.Decode.succeed
        (\data included meta ->
            { data = data, included = included, meta = meta }
        )
        |> OpenApi.Common.jsonDecodeAndMap
            (Json.Decode.field
                "data"
                (Json.Decode.list
                    (Json.Decode.succeed
                        (\attributes id relationships type_ ->
                            { attributes =
                                attributes
                            , id = id
                            , relationships =
                                relationships
                            , type_ = type_
                            }
                        )
                        |> OpenApi.Common.jsonDecodeAndMap
                            (OpenApi.Common.decodeOptionalField
                                "attributes"
                                decodeMember
                            )
                        |> OpenApi.Common.jsonDecodeAndMap
                            (Json.Decode.field
                                "id"
                                Json.Decode.string
                            )
                        |> OpenApi.Common.jsonDecodeAndMap
                            (OpenApi.Common.decodeOptionalField
                                "relationships"
                                Json.Decode.value
                            )
                        |> OpenApi.Common.jsonDecodeAndMap
                            (Json.Decode.field
                                "type"
                                Json.Decode.string
                            )
                    )
                )
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "included"
                (Json.Decode.list Json.Decode.value)
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (Json.Decode.field
                "meta"
                decodeJSONAPIResponseMeta
            )


decodePledgeEvent : Json.Decode.Decoder PatreonApi.Types.PledgeEvent
decodePledgeEvent =
    Json.Decode.succeed
        (\amount_cents currency_code date payment_status pledge_payment_status tier_id tier_title type_ ->
            { amount_cents = amount_cents
            , currency_code = currency_code
            , date = date
            , payment_status = payment_status
            , pledge_payment_status = pledge_payment_status
            , tier_id = tier_id
            , tier_title = tier_title
            , type_ = type_
            }
        )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "amount_cents"
                Json.Decode.float
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "currency_code"
                Json.Decode.string
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "date"
                OpenApi.Common.decodeStringDateTime
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "payment_status"
                Json.Decode.string
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "pledge_payment_status"
                Json.Decode.string
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "tier_id"
                (Json.Decode.oneOf
                    [ Json.Decode.map
                        OpenApi.Common.Present
                        Json.Decode.string
                    , Json.Decode.null
                        OpenApi.Common.Null
                    ]
                )
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "tier_title"
                (Json.Decode.oneOf
                    [ Json.Decode.map
                        OpenApi.Common.Present
                        Json.Decode.string
                    , Json.Decode.null
                        OpenApi.Common.Null
                    ]
                )
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "type"
                Json.Decode.string
            )


decodePost : Json.Decode.Decoder PatreonApi.Types.Post
decodePost =
    Json.Decode.succeed
        (\app_id app_status content embed_data embed_url is_paid is_public published_at tiers title url ->
            { app_id = app_id
            , app_status = app_status
            , content = content
            , embed_data = embed_data
            , embed_url = embed_url
            , is_paid = is_paid
            , is_public = is_public
            , published_at = published_at
            , tiers = tiers
            , title = title
            , url = url
            }
        )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "app_id"
                (Json.Decode.oneOf
                    [ Json.Decode.map
                        OpenApi.Common.Present
                        Json.Decode.float
                    , Json.Decode.null OpenApi.Common.Null
                    ]
                )
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "app_status"
                (Json.Decode.oneOf
                    [ Json.Decode.map
                        OpenApi.Common.Present
                        Json.Decode.string
                    , Json.Decode.null
                        OpenApi.Common.Null
                    ]
                )
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "content"
                (Json.Decode.oneOf
                    [ Json.Decode.map
                        OpenApi.Common.Present
                        Json.Decode.string
                    , Json.Decode.null
                        OpenApi.Common.Null
                    ]
                )
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "embed_data"
                (Json.Decode.oneOf
                    [ Json.Decode.map
                        OpenApi.Common.Present
                        (Json.Decode.succeed
                            {}
                        )
                    , Json.Decode.null
                        OpenApi.Common.Null
                    ]
                )
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "embed_url"
                (Json.Decode.oneOf
                    [ Json.Decode.map
                        OpenApi.Common.Present
                        OpenApi.Common.decodeStringUri
                    , Json.Decode.null
                        OpenApi.Common.Null
                    ]
                )
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "is_paid"
                (Json.Decode.oneOf
                    [ Json.Decode.map
                        OpenApi.Common.Present
                        Json.Decode.bool
                    , Json.Decode.null
                        OpenApi.Common.Null
                    ]
                )
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "is_public"
                (Json.Decode.oneOf
                    [ Json.Decode.map
                        OpenApi.Common.Present
                        Json.Decode.bool
                    , Json.Decode.null
                        OpenApi.Common.Null
                    ]
                )
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "published_at"
                (Json.Decode.oneOf
                    [ Json.Decode.map
                        OpenApi.Common.Present
                        OpenApi.Common.decodeStringDateTime
                    , Json.Decode.null
                        OpenApi.Common.Null
                    ]
                )
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "tiers"
                (Json.Decode.oneOf
                    [ Json.Decode.map
                        OpenApi.Common.Present
                        (Json.Decode.list
                            Json.Decode.string
                        )
                    , Json.Decode.null
                        OpenApi.Common.Null
                    ]
                )
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "title"
                (Json.Decode.oneOf
                    [ Json.Decode.map
                        OpenApi.Common.Present
                        Json.Decode.string
                    , Json.Decode.null
                        OpenApi.Common.Null
                    ]
                )
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "url"
                OpenApi.Common.decodeStringUri
            )


decodePostResponse : Json.Decode.Decoder PatreonApi.Types.PostResponse
decodePostResponse =
    Json.Decode.succeed
        (\data included links ->
            { data = data, included = included, links = links }
        )
        |> OpenApi.Common.jsonDecodeAndMap
            (Json.Decode.field
                "data"
                (Json.Decode.succeed
                    (\attributes id relationships type_ ->
                        { attributes = attributes
                        , id = id
                        , relationships = relationships
                        , type_ = type_
                        }
                    )
                    |> OpenApi.Common.jsonDecodeAndMap
                        (OpenApi.Common.decodeOptionalField
                            "attributes"
                            decodePost
                        )
                    |> OpenApi.Common.jsonDecodeAndMap
                        (Json.Decode.field
                            "id"
                            Json.Decode.string
                        )
                    |> OpenApi.Common.jsonDecodeAndMap
                        (OpenApi.Common.decodeOptionalField
                            "relationships"
                            Json.Decode.value
                        )
                    |> OpenApi.Common.jsonDecodeAndMap
                        (Json.Decode.field
                            "type"
                            Json.Decode.string
                        )
                )
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "included"
                (Json.Decode.list Json.Decode.value)
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (Json.Decode.field
                "links"
                decodeJSONAPIResponseLinks
            )


decodePostsResponse : Json.Decode.Decoder PatreonApi.Types.PostsResponse
decodePostsResponse =
    Json.Decode.succeed
        (\data included meta ->
            { data = data, included = included, meta = meta }
        )
        |> OpenApi.Common.jsonDecodeAndMap
            (Json.Decode.field
                "data"
                (Json.Decode.list
                    (Json.Decode.succeed
                        (\attributes id relationships type_ ->
                            { attributes =
                                attributes
                            , id = id
                            , relationships =
                                relationships
                            , type_ = type_
                            }
                        )
                        |> OpenApi.Common.jsonDecodeAndMap
                            (OpenApi.Common.decodeOptionalField
                                "attributes"
                                decodePost
                            )
                        |> OpenApi.Common.jsonDecodeAndMap
                            (Json.Decode.field
                                "id"
                                Json.Decode.string
                            )
                        |> OpenApi.Common.jsonDecodeAndMap
                            (OpenApi.Common.decodeOptionalField
                                "relationships"
                                Json.Decode.value
                            )
                        |> OpenApi.Common.jsonDecodeAndMap
                            (Json.Decode.field
                                "type"
                                Json.Decode.string
                            )
                    )
                )
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "included"
                (Json.Decode.list Json.Decode.value)
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (Json.Decode.field
                "meta"
                decodeJSONAPIResponseMeta
            )


decodeStatusCode400 : Json.Decode.Decoder PatreonApi.Types.StatusCode400
decodeStatusCode400 =
    Json.Decode.list decodeJSONAPIError


decodeStatusCode401 : Json.Decode.Decoder PatreonApi.Types.StatusCode401
decodeStatusCode401 =
    Json.Decode.list decodeJSONAPIError


decodeStatusCode403 : Json.Decode.Decoder PatreonApi.Types.StatusCode403
decodeStatusCode403 =
    Json.Decode.list decodeJSONAPIError


decodeStatusCode404 : Json.Decode.Decoder PatreonApi.Types.StatusCode404
decodeStatusCode404 =
    Json.Decode.list decodeJSONAPIError


decodeStatusCode405 : Json.Decode.Decoder PatreonApi.Types.StatusCode405
decodeStatusCode405 =
    Json.Decode.list decodeJSONAPIError


decodeStatusCode406 : Json.Decode.Decoder PatreonApi.Types.StatusCode406
decodeStatusCode406 =
    Json.Decode.list decodeJSONAPIError


decodeStatusCode410 : Json.Decode.Decoder PatreonApi.Types.StatusCode410
decodeStatusCode410 =
    Json.Decode.list decodeJSONAPIError


decodeStatusCode429 : Json.Decode.Decoder PatreonApi.Types.StatusCode429
decodeStatusCode429 =
    Json.Decode.list decodeJSONAPIError


decodeStatusCode500 : Json.Decode.Decoder PatreonApi.Types.StatusCode500
decodeStatusCode500 =
    Json.Decode.list decodeJSONAPIError


decodeStatusCode503 : Json.Decode.Decoder PatreonApi.Types.StatusCode503
decodeStatusCode503 =
    Json.Decode.list decodeJSONAPIError


decodeTier : Json.Decode.Decoder PatreonApi.Types.Tier
decodeTier =
    Json.Decode.succeed
        (\amount_cents created_at description discord_role_ids edited_at image_url patron_count post_count published published_at remaining requires_shipping title unpublished_at url user_limit ->
            { amount_cents = amount_cents
            , created_at = created_at
            , description = description
            , discord_role_ids = discord_role_ids
            , edited_at = edited_at
            , image_url = image_url
            , patron_count = patron_count
            , post_count = post_count
            , published = published
            , published_at = published_at
            , remaining = remaining
            , requires_shipping = requires_shipping
            , title = title
            , unpublished_at = unpublished_at
            , url = url
            , user_limit = user_limit
            }
        )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "amount_cents"
                Json.Decode.float
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "created_at"
                OpenApi.Common.decodeStringDateTime
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "description"
                Json.Decode.string
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "discord_role_ids"
                (Json.Decode.oneOf
                    [ Json.Decode.map
                        OpenApi.Common.Present
                        (Json.Decode.list
                            Json.Decode.string
                        )
                    , Json.Decode.null
                        OpenApi.Common.Null
                    ]
                )
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "edited_at"
                OpenApi.Common.decodeStringDateTime
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "image_url"
                (Json.Decode.oneOf
                    [ Json.Decode.map
                        OpenApi.Common.Present
                        OpenApi.Common.decodeStringUri
                    , Json.Decode.null
                        OpenApi.Common.Null
                    ]
                )
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "patron_count"
                Json.Decode.float
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "post_count"
                Json.Decode.float
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "published"
                Json.Decode.bool
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "published_at"
                (Json.Decode.oneOf
                    [ Json.Decode.map
                        OpenApi.Common.Present
                        OpenApi.Common.decodeStringDateTime
                    , Json.Decode.null
                        OpenApi.Common.Null
                    ]
                )
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "remaining"
                (Json.Decode.oneOf
                    [ Json.Decode.map
                        OpenApi.Common.Present
                        Json.Decode.float
                    , Json.Decode.null
                        OpenApi.Common.Null
                    ]
                )
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "requires_shipping"
                Json.Decode.bool
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "title"
                Json.Decode.string
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "unpublished_at"
                (Json.Decode.oneOf
                    [ Json.Decode.map
                        OpenApi.Common.Present
                        OpenApi.Common.decodeStringDateTime
                    , Json.Decode.null
                        OpenApi.Common.Null
                    ]
                )
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "url"
                OpenApi.Common.decodeStringUri
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "user_limit"
                (Json.Decode.oneOf
                    [ Json.Decode.map
                        OpenApi.Common.Present
                        Json.Decode.float
                    , Json.Decode.null
                        OpenApi.Common.Null
                    ]
                )
            )


decodeUser : Json.Decode.Decoder PatreonApi.Types.User
decodeUser =
    Json.Decode.succeed
        (\about can_see_nsfw created email first_name full_name hide_pledges image_url is_creator is_email_verified last_name like_count social_connections thumb_url url vanity ->
            { about = about
            , can_see_nsfw = can_see_nsfw
            , created = created
            , email = email
            , first_name = first_name
            , full_name = full_name
            , hide_pledges = hide_pledges
            , image_url = image_url
            , is_creator = is_creator
            , is_email_verified = is_email_verified
            , last_name = last_name
            , like_count = like_count
            , social_connections = social_connections
            , thumb_url = thumb_url
            , url = url
            , vanity = vanity
            }
        )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "about"
                (Json.Decode.oneOf
                    [ Json.Decode.map
                        OpenApi.Common.Present
                        Json.Decode.string
                    , Json.Decode.null OpenApi.Common.Null
                    ]
                )
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "can_see_nsfw"
                (Json.Decode.oneOf
                    [ Json.Decode.map
                        OpenApi.Common.Present
                        Json.Decode.bool
                    , Json.Decode.null
                        OpenApi.Common.Null
                    ]
                )
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "created"
                OpenApi.Common.decodeStringDateTime
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "email"
                Json.Decode.string
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "first_name"
                (Json.Decode.oneOf
                    [ Json.Decode.map
                        OpenApi.Common.Present
                        Json.Decode.string
                    , Json.Decode.null
                        OpenApi.Common.Null
                    ]
                )
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "full_name"
                Json.Decode.string
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "hide_pledges"
                (Json.Decode.oneOf
                    [ Json.Decode.map
                        OpenApi.Common.Present
                        Json.Decode.bool
                    , Json.Decode.null
                        OpenApi.Common.Null
                    ]
                )
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "image_url"
                OpenApi.Common.decodeStringUri
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "is_creator"
                Json.Decode.bool
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "is_email_verified"
                Json.Decode.bool
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "last_name"
                (Json.Decode.oneOf
                    [ Json.Decode.map
                        OpenApi.Common.Present
                        Json.Decode.string
                    , Json.Decode.null
                        OpenApi.Common.Null
                    ]
                )
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "like_count"
                Json.Decode.float
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "social_connections"
                (Json.Decode.succeed
                    {}
                )
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "thumb_url"
                OpenApi.Common.decodeStringUri
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "url"
                OpenApi.Common.decodeStringUri
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "vanity"
                (Json.Decode.oneOf
                    [ Json.Decode.map
                        OpenApi.Common.Present
                        Json.Decode.string
                    , Json.Decode.null
                        OpenApi.Common.Null
                    ]
                )
            )


decodeUserResponse : Json.Decode.Decoder PatreonApi.Types.UserResponse
decodeUserResponse =
    Json.Decode.succeed
        (\data included links ->
            { data = data, included = included, links = links }
        )
        |> OpenApi.Common.jsonDecodeAndMap
            (Json.Decode.field
                "data"
                (Json.Decode.succeed
                    (\attributes id relationships type_ ->
                        { attributes = attributes
                        , id = id
                        , relationships = relationships
                        , type_ = type_
                        }
                    )
                    |> OpenApi.Common.jsonDecodeAndMap
                        (OpenApi.Common.decodeOptionalField
                            "attributes"
                            decodeUser
                        )
                    |> OpenApi.Common.jsonDecodeAndMap
                        (Json.Decode.field
                            "id"
                            Json.Decode.string
                        )
                    |> OpenApi.Common.jsonDecodeAndMap
                        (OpenApi.Common.decodeOptionalField
                            "relationships"
                            Json.Decode.value
                        )
                    |> OpenApi.Common.jsonDecodeAndMap
                        (Json.Decode.field
                            "type"
                            Json.Decode.string
                        )
                )
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "included"
                (Json.Decode.list Json.Decode.value)
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (Json.Decode.field
                "links"
                decodeJSONAPIResponseLinks
            )


decodeWebhook : Json.Decode.Decoder PatreonApi.Types.Webhook
decodeWebhook =
    Json.Decode.succeed
        (\last_attempted_at num_consecutive_times_failed paused secret triggers uri ->
            { last_attempted_at = last_attempted_at
            , num_consecutive_times_failed = num_consecutive_times_failed
            , paused = paused
            , secret = secret
            , triggers = triggers
            , uri = uri
            }
        )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "last_attempted_at"
                OpenApi.Common.decodeStringDateTime
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "num_consecutive_times_failed"
                Json.Decode.float
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "paused"
                Json.Decode.bool
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "secret"
                Json.Decode.string
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "triggers"
                (Json.Decode.list
                    Json.Decode.string
                )
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "uri"
                OpenApi.Common.decodeStringUri
            )


decodeWebhookResponse : Json.Decode.Decoder PatreonApi.Types.WebhookResponse
decodeWebhookResponse =
    Json.Decode.succeed
        (\data included links ->
            { data = data, included = included, links = links }
        )
        |> OpenApi.Common.jsonDecodeAndMap
            (Json.Decode.field
                "data"
                (Json.Decode.succeed
                    (\attributes id relationships type_ ->
                        { attributes = attributes
                        , id = id
                        , relationships = relationships
                        , type_ = type_
                        }
                    )
                    |> OpenApi.Common.jsonDecodeAndMap
                        (OpenApi.Common.decodeOptionalField
                            "attributes"
                            decodeWebhook
                        )
                    |> OpenApi.Common.jsonDecodeAndMap
                        (Json.Decode.field
                            "id"
                            Json.Decode.string
                        )
                    |> OpenApi.Common.jsonDecodeAndMap
                        (OpenApi.Common.decodeOptionalField
                            "relationships"
                            Json.Decode.value
                        )
                    |> OpenApi.Common.jsonDecodeAndMap
                        (Json.Decode.field
                            "type"
                            Json.Decode.string
                        )
                )
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "included"
                (Json.Decode.list Json.Decode.value)
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (Json.Decode.field
                "links"
                decodeJSONAPIResponseLinks
            )


decodeWebhookTrigger : Json.Decode.Decoder PatreonApi.Types.WebhookTrigger
decodeWebhookTrigger =
    Json.Decode.list Json.Decode.string


decodeWebhooksResponse : Json.Decode.Decoder PatreonApi.Types.WebhooksResponse
decodeWebhooksResponse =
    Json.Decode.succeed
        (\data included meta ->
            { data = data, included = included, meta = meta }
        )
        |> OpenApi.Common.jsonDecodeAndMap
            (Json.Decode.field
                "data"
                (Json.Decode.list
                    (Json.Decode.succeed
                        (\attributes id relationships type_ ->
                            { attributes =
                                attributes
                            , id = id
                            , relationships =
                                relationships
                            , type_ = type_
                            }
                        )
                        |> OpenApi.Common.jsonDecodeAndMap
                            (OpenApi.Common.decodeOptionalField
                                "attributes"
                                decodeWebhook
                            )
                        |> OpenApi.Common.jsonDecodeAndMap
                            (Json.Decode.field
                                "id"
                                Json.Decode.string
                            )
                        |> OpenApi.Common.jsonDecodeAndMap
                            (OpenApi.Common.decodeOptionalField
                                "relationships"
                                Json.Decode.value
                            )
                        |> OpenApi.Common.jsonDecodeAndMap
                            (Json.Decode.field
                                "type"
                                Json.Decode.string
                            )
                    )
                )
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "included"
                (Json.Decode.list Json.Decode.value)
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (Json.Decode.field
                "meta"
                decodeJSONAPIResponseMeta
            )


encodeAddress : PatreonApi.Types.Address -> Json.Encode.Value
encodeAddress rec =
    Json.Encode.object
        (List.filterMap
            Basics.identity
            [ Maybe.map
                (\mapUnpack ->
                    ( "addressee"
                    , case mapUnpack of
                        OpenApi.Common.Null ->
                            Json.Encode.null

                        OpenApi.Common.Present value ->
                            Json.Encode.string value
                    )
                )
                rec.addressee
            , Maybe.map
                (\mapUnpack -> ( "city", Json.Encode.string mapUnpack ))
                rec.city
            , Maybe.map
                (\mapUnpack -> ( "country", Json.Encode.string mapUnpack ))
                rec.country
            , Maybe.map
                (\mapUnpack ->
                    ( "created_at"
                    , OpenApi.Common.encodeStringDateTime mapUnpack
                    )
                )
                rec.created_at
            , Maybe.map
                (\mapUnpack ->
                    ( "line_1"
                    , case mapUnpack of
                        OpenApi.Common.Null ->
                            Json.Encode.null

                        OpenApi.Common.Present value ->
                            Json.Encode.string value
                    )
                )
                rec.line_1
            , Maybe.map
                (\mapUnpack ->
                    ( "line_2"
                    , case mapUnpack of
                        OpenApi.Common.Null ->
                            Json.Encode.null

                        OpenApi.Common.Present value ->
                            Json.Encode.string value
                    )
                )
                rec.line_2
            , Maybe.map
                (\mapUnpack ->
                    ( "phone_number"
                    , case mapUnpack of
                        OpenApi.Common.Null ->
                            Json.Encode.null

                        OpenApi.Common.Present value ->
                            Json.Encode.string value
                    )
                )
                rec.phone_number
            , Maybe.map
                (\mapUnpack ->
                    ( "postal_code"
                    , case mapUnpack of
                        OpenApi.Common.Null ->
                            Json.Encode.null

                        OpenApi.Common.Present value ->
                            Json.Encode.string value
                    )
                )
                rec.postal_code
            , Maybe.map
                (\mapUnpack ->
                    ( "state"
                    , case mapUnpack of
                        OpenApi.Common.Null ->
                            Json.Encode.null

                        OpenApi.Common.Present value ->
                            Json.Encode.string value
                    )
                )
                rec.state
            ]
        )


encodeBenefit : PatreonApi.Types.Benefit -> Json.Encode.Value
encodeBenefit rec =
    Json.Encode.object
        (List.filterMap
            Basics.identity
            [ Maybe.map
                (\mapUnpack ->
                    ( "app_external_id"
                    , case mapUnpack of
                        OpenApi.Common.Null ->
                            Json.Encode.null

                        OpenApi.Common.Present value ->
                            Json.Encode.string value
                    )
                )
                rec.app_external_id
            , Maybe.map
                (\mapUnpack ->
                    ( "app_meta"
                    , case mapUnpack of
                        OpenApi.Common.Null ->
                            Json.Encode.null

                        OpenApi.Common.Present value ->
                            Json.Encode.object []
                    )
                )
                rec.app_meta
            , Maybe.map
                (\mapUnpack ->
                    ( "benefit_type"
                    , case mapUnpack of
                        OpenApi.Common.Null ->
                            Json.Encode.null

                        OpenApi.Common.Present value ->
                            Json.Encode.string value
                    )
                )
                rec.benefit_type
            , Maybe.map
                (\mapUnpack ->
                    ( "created_at"
                    , OpenApi.Common.encodeStringDateTime mapUnpack
                    )
                )
                rec.created_at
            , Maybe.map
                (\mapUnpack ->
                    ( "deliverables_due_today_count"
                    , Json.Encode.float mapUnpack
                    )
                )
                rec.deliverables_due_today_count
            , Maybe.map
                (\mapUnpack ->
                    ( "delivered_deliverables_count"
                    , Json.Encode.float mapUnpack
                    )
                )
                rec.delivered_deliverables_count
            , Maybe.map
                (\mapUnpack ->
                    ( "description"
                    , case mapUnpack of
                        OpenApi.Common.Null ->
                            Json.Encode.null

                        OpenApi.Common.Present value ->
                            Json.Encode.string value
                    )
                )
                rec.description
            , Maybe.map
                (\mapUnpack -> ( "is_deleted", Json.Encode.bool mapUnpack ))
                rec.is_deleted
            , Maybe.map
                (\mapUnpack -> ( "is_ended", Json.Encode.bool mapUnpack ))
                rec.is_ended
            , Maybe.map
                (\mapUnpack -> ( "is_published", Json.Encode.bool mapUnpack ))
                rec.is_published
            , Maybe.map
                (\mapUnpack ->
                    ( "next_deliverable_due_date"
                    , case mapUnpack of
                        OpenApi.Common.Null ->
                            Json.Encode.null

                        OpenApi.Common.Present value ->
                            OpenApi.Common.encodeStringDateTime value
                    )
                )
                rec.next_deliverable_due_date
            , Maybe.map
                (\mapUnpack ->
                    ( "not_delivered_deliverables_count"
                    , Json.Encode.float mapUnpack
                    )
                )
                rec.not_delivered_deliverables_count
            , Maybe.map
                (\mapUnpack ->
                    ( "rule_type"
                    , case mapUnpack of
                        OpenApi.Common.Null ->
                            Json.Encode.null

                        OpenApi.Common.Present value ->
                            Json.Encode.string value
                    )
                )
                rec.rule_type
            , Maybe.map
                (\mapUnpack -> ( "tiers_count", Json.Encode.float mapUnpack ))
                rec.tiers_count
            , Maybe.map
                (\mapUnpack -> ( "title", Json.Encode.string mapUnpack ))
                rec.title
            ]
        )


encodeCampaign : PatreonApi.Types.Campaign -> Json.Encode.Value
encodeCampaign rec =
    Json.Encode.object
        (List.filterMap
            Basics.identity
            [ Maybe.map
                (\mapUnpack ->
                    ( "created_at"
                    , OpenApi.Common.encodeStringDateTime mapUnpack
                    )
                )
                rec.created_at
            , Maybe.map
                (\mapUnpack ->
                    ( "creation_name"
                    , case mapUnpack of
                        OpenApi.Common.Null ->
                            Json.Encode.null

                        OpenApi.Common.Present value ->
                            Json.Encode.string value
                    )
                )
                rec.creation_name
            , Maybe.map
                (\mapUnpack ->
                    ( "discord_server_id"
                    , case mapUnpack of
                        OpenApi.Common.Null ->
                            Json.Encode.null

                        OpenApi.Common.Present value ->
                            Json.Encode.string value
                    )
                )
                rec.discord_server_id
            , Maybe.map
                (\mapUnpack ->
                    ( "google_analytics_id"
                    , case mapUnpack of
                        OpenApi.Common.Null ->
                            Json.Encode.null

                        OpenApi.Common.Present value ->
                            Json.Encode.string value
                    )
                )
                rec.google_analytics_id
            , Maybe.map
                (\mapUnpack -> ( "has_rss", Json.Encode.bool mapUnpack ))
                rec.has_rss
            , Maybe.map
                (\mapUnpack ->
                    ( "has_sent_rss_notify", Json.Encode.bool mapUnpack )
                )
                rec.has_sent_rss_notify
            , Maybe.map
                (\mapUnpack ->
                    ( "image_small_url"
                    , OpenApi.Common.encodeStringUri mapUnpack
                    )
                )
                rec.image_small_url
            , Maybe.map
                (\mapUnpack ->
                    ( "image_url", OpenApi.Common.encodeStringUri mapUnpack )
                )
                rec.image_url
            , Maybe.map
                (\mapUnpack ->
                    ( "is_charged_immediately", Json.Encode.bool mapUnpack )
                )
                rec.is_charged_immediately
            , Maybe.map
                (\mapUnpack -> ( "is_monthly", Json.Encode.bool mapUnpack ))
                rec.is_monthly
            , Maybe.map
                (\mapUnpack -> ( "is_nsfw", Json.Encode.bool mapUnpack ))
                rec.is_nsfw
            , Maybe.map
                (\mapUnpack ->
                    ( "main_video_embed"
                    , case mapUnpack of
                        OpenApi.Common.Null ->
                            Json.Encode.null

                        OpenApi.Common.Present value ->
                            Json.Encode.string value
                    )
                )
                rec.main_video_embed
            , Maybe.map
                (\mapUnpack ->
                    ( "main_video_url"
                    , case mapUnpack of
                        OpenApi.Common.Null ->
                            Json.Encode.null

                        OpenApi.Common.Present value ->
                            OpenApi.Common.encodeStringUri value
                    )
                )
                rec.main_video_url
            , Maybe.map
                (\mapUnpack ->
                    ( "one_liner"
                    , case mapUnpack of
                        OpenApi.Common.Null ->
                            Json.Encode.null

                        OpenApi.Common.Present value ->
                            Json.Encode.string value
                    )
                )
                rec.one_liner
            , Maybe.map
                (\mapUnpack -> ( "patron_count", Json.Encode.float mapUnpack ))
                rec.patron_count
            , Maybe.map
                (\mapUnpack ->
                    ( "pay_per_name"
                    , case mapUnpack of
                        OpenApi.Common.Null ->
                            Json.Encode.null

                        OpenApi.Common.Present value ->
                            Json.Encode.string value
                    )
                )
                rec.pay_per_name
            , Maybe.map
                (\mapUnpack -> ( "pledge_url", Json.Encode.string mapUnpack ))
                rec.pledge_url
            , Maybe.map
                (\mapUnpack ->
                    ( "published_at"
                    , case mapUnpack of
                        OpenApi.Common.Null ->
                            Json.Encode.null

                        OpenApi.Common.Present value ->
                            OpenApi.Common.encodeStringDateTime value
                    )
                )
                rec.published_at
            , Maybe.map
                (\mapUnpack ->
                    ( "rss_artwork_url"
                    , case mapUnpack of
                        OpenApi.Common.Null ->
                            Json.Encode.null

                        OpenApi.Common.Present value ->
                            OpenApi.Common.encodeStringUri value
                    )
                )
                rec.rss_artwork_url
            , Maybe.map
                (\mapUnpack ->
                    ( "rss_feed_title"
                    , case mapUnpack of
                        OpenApi.Common.Null ->
                            Json.Encode.null

                        OpenApi.Common.Present value ->
                            Json.Encode.string value
                    )
                )
                rec.rss_feed_title
            , Maybe.map
                (\mapUnpack -> ( "show_earnings", Json.Encode.bool mapUnpack ))
                rec.show_earnings
            , Maybe.map
                (\mapUnpack ->
                    ( "summary"
                    , case mapUnpack of
                        OpenApi.Common.Null ->
                            Json.Encode.null

                        OpenApi.Common.Present value ->
                            Json.Encode.string value
                    )
                )
                rec.summary
            , Maybe.map
                (\mapUnpack ->
                    ( "thanks_embed"
                    , case mapUnpack of
                        OpenApi.Common.Null ->
                            Json.Encode.null

                        OpenApi.Common.Present value ->
                            Json.Encode.string value
                    )
                )
                rec.thanks_embed
            , Maybe.map
                (\mapUnpack ->
                    ( "thanks_msg"
                    , case mapUnpack of
                        OpenApi.Common.Null ->
                            Json.Encode.null

                        OpenApi.Common.Present value ->
                            Json.Encode.string value
                    )
                )
                rec.thanks_msg
            , Maybe.map
                (\mapUnpack ->
                    ( "thanks_video_url"
                    , case mapUnpack of
                        OpenApi.Common.Null ->
                            Json.Encode.null

                        OpenApi.Common.Present value ->
                            OpenApi.Common.encodeStringUri value
                    )
                )
                rec.thanks_video_url
            , Maybe.map
                (\mapUnpack ->
                    ( "url", OpenApi.Common.encodeStringUri mapUnpack )
                )
                rec.url
            , Maybe.map
                (\mapUnpack ->
                    ( "vanity"
                    , case mapUnpack of
                        OpenApi.Common.Null ->
                            Json.Encode.null

                        OpenApi.Common.Present value ->
                            Json.Encode.string value
                    )
                )
                rec.vanity
            ]
        )


encodeCampaignResponse : PatreonApi.Types.CampaignResponse -> Json.Encode.Value
encodeCampaignResponse rec =
    Json.Encode.object
        (List.filterMap
            Basics.identity
            [ Just
                ( "data"
                , Json.Encode.object
                    (List.filterMap
                        Basics.identity
                        [ Maybe.map
                            (\mapUnpack ->
                                ( "attributes", encodeCampaign mapUnpack )
                            )
                            rec.data.attributes
                        , Just ( "id", Json.Encode.string rec.data.id )
                        , Maybe.map
                            (\mapUnpack ->
                                ( "relationships"
                                , Basics.identity mapUnpack
                                )
                            )
                            rec.data.relationships
                        , Just ( "type", Json.Encode.string rec.data.type_ )
                        ]
                    )
                )
            , Maybe.map
                (\mapUnpack ->
                    ( "included", Json.Encode.list Basics.identity mapUnpack )
                )
                rec.included
            , Just ( "links", encodeJSONAPIResponseLinks rec.links )
            ]
        )


encodeCampaignsResponse : PatreonApi.Types.CampaignsResponse -> Json.Encode.Value
encodeCampaignsResponse rec =
    Json.Encode.object
        (List.filterMap
            Basics.identity
            [ Just
                ( "data"
                , Json.Encode.list
                    (\rec0 ->
                        Json.Encode.object
                            (List.filterMap
                                Basics.identity
                                [ Maybe.map
                                    (\mapUnpack ->
                                        ( "attributes"
                                        , encodeCampaign mapUnpack
                                        )
                                    )
                                    rec0.attributes
                                , Just ( "id", Json.Encode.string rec0.id )
                                , Maybe.map
                                    (\mapUnpack ->
                                        ( "relationships"
                                        , Basics.identity mapUnpack
                                        )
                                    )
                                    rec0.relationships
                                , Just
                                    ( "type"
                                    , Json.Encode.string rec0.type_
                                    )
                                ]
                            )
                    )
                    rec.data
                )
            , Maybe.map
                (\mapUnpack ->
                    ( "included", Json.Encode.list Basics.identity mapUnpack )
                )
                rec.included
            , Just ( "meta", encodeJSONAPIResponseMeta rec.meta )
            ]
        )


encodeClient : PatreonApi.Types.Client -> Json.Encode.Value
encodeClient rec =
    Json.Encode.object
        (List.filterMap
            Basics.identity
            [ Maybe.map
                (\mapUnpack ->
                    ( "author_name"
                    , case mapUnpack of
                        OpenApi.Common.Null ->
                            Json.Encode.null

                        OpenApi.Common.Present value ->
                            Json.Encode.string value
                    )
                )
                rec.author_name
            , Maybe.map
                (\mapUnpack -> ( "category", Json.Encode.string mapUnpack ))
                rec.category
            , Maybe.map
                (\mapUnpack ->
                    ( "client_secret", Json.Encode.string mapUnpack )
                )
                rec.client_secret
            , Maybe.map
                (\mapUnpack ->
                    ( "default_scopes", Json.Encode.string mapUnpack )
                )
                rec.default_scopes
            , Maybe.map
                (\mapUnpack -> ( "description", Json.Encode.string mapUnpack ))
                rec.description
            , Maybe.map
                (\mapUnpack ->
                    ( "domain"
                    , case mapUnpack of
                        OpenApi.Common.Null ->
                            Json.Encode.null

                        OpenApi.Common.Present value ->
                            OpenApi.Common.encodeStringUri value
                    )
                )
                rec.domain
            , Maybe.map
                (\mapUnpack ->
                    ( "icon_url"
                    , case mapUnpack of
                        OpenApi.Common.Null ->
                            Json.Encode.null

                        OpenApi.Common.Present value ->
                            OpenApi.Common.encodeStringUri value
                    )
                )
                rec.icon_url
            , Maybe.map
                (\mapUnpack -> ( "name", Json.Encode.string mapUnpack ))
                rec.name
            , Maybe.map
                (\mapUnpack ->
                    ( "privacy_policy_url"
                    , case mapUnpack of
                        OpenApi.Common.Null ->
                            Json.Encode.null

                        OpenApi.Common.Present value ->
                            OpenApi.Common.encodeStringUri value
                    )
                )
                rec.privacy_policy_url
            , Maybe.map
                (\mapUnpack ->
                    ( "redirect_uris", Json.Encode.string mapUnpack )
                )
                rec.redirect_uris
            , Maybe.map
                (\mapUnpack ->
                    ( "tos_url", OpenApi.Common.encodeStringUri mapUnpack )
                )
                rec.tos_url
            , Maybe.map
                (\mapUnpack -> ( "version", Json.Encode.float mapUnpack ))
                rec.version
            ]
        )


encodeDeliverable : PatreonApi.Types.Deliverable -> Json.Encode.Value
encodeDeliverable rec =
    Json.Encode.object
        (List.filterMap
            Basics.identity
            [ Maybe.map
                (\mapUnpack ->
                    ( "completed_at"
                    , case mapUnpack of
                        OpenApi.Common.Null ->
                            Json.Encode.null

                        OpenApi.Common.Present value ->
                            OpenApi.Common.encodeStringDateTime value
                    )
                )
                rec.completed_at
            , Maybe.map
                (\mapUnpack ->
                    ( "delivery_status", Json.Encode.string mapUnpack )
                )
                rec.delivery_status
            , Maybe.map
                (\mapUnpack ->
                    ( "due_at", OpenApi.Common.encodeStringDateTime mapUnpack )
                )
                rec.due_at
            ]
        )


encodeGoal : PatreonApi.Types.Goal -> Json.Encode.Value
encodeGoal rec =
    Json.Encode.object []


encodeJSONAPIError : PatreonApi.Types.JSONAPIError -> Json.Encode.Value
encodeJSONAPIError rec =
    Json.Encode.object
        (List.filterMap
            Basics.identity
            [ Maybe.map
                (\mapUnpack -> ( "code", Json.Encode.float mapUnpack ))
                rec.code
            , Maybe.map
                (\mapUnpack -> ( "code_name", Json.Encode.string mapUnpack ))
                rec.code_name
            , Maybe.map
                (\mapUnpack -> ( "detail", Json.Encode.string mapUnpack ))
                rec.detail
            , Maybe.map
                (\mapUnpack -> ( "id", Json.Encode.string mapUnpack ))
                rec.id
            , Maybe.map
                (\mapUnpack -> ( "status", Json.Encode.string mapUnpack ))
                rec.status
            , Maybe.map
                (\mapUnpack -> ( "title", Json.Encode.string mapUnpack ))
                rec.title
            ]
        )


encodeJSONAPILinksRelated : PatreonApi.Types.JSONAPILinksRelated -> Json.Encode.Value
encodeJSONAPILinksRelated =
    Basics.identity


encodeJSONAPIResource : PatreonApi.Types.JSONAPIResource -> Json.Encode.Value
encodeJSONAPIResource rec =
    Json.Encode.object
        [ ( "id", Json.Encode.string rec.id )
        , ( "type", Json.Encode.string rec.type_ )
        ]


encodeJSONAPIResponseLinks : PatreonApi.Types.JSONAPIResponseLinks -> Json.Encode.Value
encodeJSONAPIResponseLinks =
    Basics.identity


encodeJSONAPIResponseMeta : PatreonApi.Types.JSONAPIResponseMeta -> Json.Encode.Value
encodeJSONAPIResponseMeta =
    Basics.identity


encodeMedia : PatreonApi.Types.Media -> Json.Encode.Value
encodeMedia rec =
    Json.Encode.object
        (List.filterMap
            Basics.identity
            [ Maybe.map
                (\mapUnpack ->
                    ( "created_at"
                    , OpenApi.Common.encodeStringDateTime mapUnpack
                    )
                )
                rec.created_at
            , Maybe.map
                (\mapUnpack ->
                    ( "download_url", OpenApi.Common.encodeStringUri mapUnpack )
                )
                rec.download_url
            , Maybe.map
                (\mapUnpack -> ( "file_name", Json.Encode.string mapUnpack ))
                rec.file_name
            , Maybe.map
                (\mapUnpack -> ( "image_urls", Json.Encode.object [] ))
                rec.image_urls
            , Maybe.map
                (\mapUnpack ->
                    ( "metadata"
                    , case mapUnpack of
                        OpenApi.Common.Null ->
                            Json.Encode.null

                        OpenApi.Common.Present value ->
                            Json.Encode.object []
                    )
                )
                rec.metadata
            , Maybe.map
                (\mapUnpack -> ( "mimetype", Json.Encode.string mapUnpack ))
                rec.mimetype
            , Maybe.map
                (\mapUnpack -> ( "owner_id", Json.Encode.string mapUnpack ))
                rec.owner_id
            , Maybe.map
                (\mapUnpack ->
                    ( "owner_relationship", Json.Encode.string mapUnpack )
                )
                rec.owner_relationship
            , Maybe.map
                (\mapUnpack -> ( "owner_type", Json.Encode.string mapUnpack ))
                rec.owner_type
            , Maybe.map
                (\mapUnpack -> ( "size_bytes", Json.Encode.float mapUnpack ))
                rec.size_bytes
            , Maybe.map
                (\mapUnpack -> ( "state", Json.Encode.string mapUnpack ))
                rec.state
            , Maybe.map
                (\mapUnpack ->
                    ( "upload_expires_at"
                    , OpenApi.Common.encodeStringDateTime mapUnpack
                    )
                )
                rec.upload_expires_at
            , Maybe.map
                (\mapUnpack -> ( "upload_parameters", Json.Encode.object [] ))
                rec.upload_parameters
            , Maybe.map
                (\mapUnpack ->
                    ( "upload_url", OpenApi.Common.encodeStringUri mapUnpack )
                )
                rec.upload_url
            ]
        )


encodeMember : PatreonApi.Types.Member -> Json.Encode.Value
encodeMember rec =
    Json.Encode.object
        (List.filterMap
            Basics.identity
            [ Maybe.map
                (\mapUnpack ->
                    ( "campaign_lifetime_support_cents"
                    , Json.Encode.float mapUnpack
                    )
                )
                rec.campaign_lifetime_support_cents
            , Maybe.map
                (\mapUnpack ->
                    ( "currently_entitled_amount_cents"
                    , Json.Encode.float mapUnpack
                    )
                )
                rec.currently_entitled_amount_cents
            , Maybe.map
                (\mapUnpack -> ( "email", Json.Encode.string mapUnpack ))
                rec.email
            , Maybe.map
                (\mapUnpack -> ( "full_name", Json.Encode.string mapUnpack ))
                rec.full_name
            , Maybe.map
                (\mapUnpack -> ( "is_follower", Json.Encode.bool mapUnpack ))
                rec.is_follower
            , Maybe.map
                (\mapUnpack -> ( "is_free_trial", Json.Encode.bool mapUnpack ))
                rec.is_free_trial
            , Maybe.map
                (\mapUnpack -> ( "is_gifted", Json.Encode.bool mapUnpack ))
                rec.is_gifted
            , Maybe.map
                (\mapUnpack ->
                    ( "last_charge_date"
                    , case mapUnpack of
                        OpenApi.Common.Null ->
                            Json.Encode.null

                        OpenApi.Common.Present value ->
                            OpenApi.Common.encodeStringDateTime value
                    )
                )
                rec.last_charge_date
            , Maybe.map
                (\mapUnpack ->
                    ( "last_charge_status"
                    , case mapUnpack of
                        OpenApi.Common.Null ->
                            Json.Encode.null

                        OpenApi.Common.Present value ->
                            Json.Encode.string value
                    )
                )
                rec.last_charge_status
            , Maybe.map
                (\mapUnpack ->
                    ( "lifetime_support_cents", Json.Encode.float mapUnpack )
                )
                rec.lifetime_support_cents
            , Maybe.map
                (\mapUnpack ->
                    ( "next_charge_date"
                    , case mapUnpack of
                        OpenApi.Common.Null ->
                            Json.Encode.null

                        OpenApi.Common.Present value ->
                            OpenApi.Common.encodeStringDateTime value
                    )
                )
                rec.next_charge_date
            , Maybe.map
                (\mapUnpack -> ( "note", Json.Encode.string mapUnpack ))
                rec.note
            , Maybe.map
                (\mapUnpack ->
                    ( "patron_status"
                    , case mapUnpack of
                        OpenApi.Common.Null ->
                            Json.Encode.null

                        OpenApi.Common.Present value ->
                            Json.Encode.string value
                    )
                )
                rec.patron_status
            , Maybe.map
                (\mapUnpack ->
                    ( "pledge_cadence"
                    , case mapUnpack of
                        OpenApi.Common.Null ->
                            Json.Encode.null

                        OpenApi.Common.Present value ->
                            Json.Encode.float value
                    )
                )
                rec.pledge_cadence
            , Maybe.map
                (\mapUnpack ->
                    ( "pledge_relationship_start"
                    , case mapUnpack of
                        OpenApi.Common.Null ->
                            Json.Encode.null

                        OpenApi.Common.Present value ->
                            OpenApi.Common.encodeStringDateTime value
                    )
                )
                rec.pledge_relationship_start
            , Maybe.map
                (\mapUnpack ->
                    ( "will_pay_amount_cents", Json.Encode.float mapUnpack )
                )
                rec.will_pay_amount_cents
            ]
        )


encodeMemberResponse : PatreonApi.Types.MemberResponse -> Json.Encode.Value
encodeMemberResponse rec =
    Json.Encode.object
        (List.filterMap
            Basics.identity
            [ Just
                ( "data"
                , Json.Encode.object
                    (List.filterMap
                        Basics.identity
                        [ Maybe.map
                            (\mapUnpack ->
                                ( "attributes", encodeMember mapUnpack )
                            )
                            rec.data.attributes
                        , Just ( "id", Json.Encode.string rec.data.id )
                        , Maybe.map
                            (\mapUnpack ->
                                ( "relationships"
                                , Basics.identity mapUnpack
                                )
                            )
                            rec.data.relationships
                        , Just ( "type", Json.Encode.string rec.data.type_ )
                        ]
                    )
                )
            , Maybe.map
                (\mapUnpack ->
                    ( "included", Json.Encode.list Basics.identity mapUnpack )
                )
                rec.included
            , Just ( "links", encodeJSONAPIResponseLinks rec.links )
            ]
        )


encodeMembersResponse : PatreonApi.Types.MembersResponse -> Json.Encode.Value
encodeMembersResponse rec =
    Json.Encode.object
        (List.filterMap
            Basics.identity
            [ Just
                ( "data"
                , Json.Encode.list
                    (\rec0 ->
                        Json.Encode.object
                            (List.filterMap
                                Basics.identity
                                [ Maybe.map
                                    (\mapUnpack ->
                                        ( "attributes"
                                        , encodeMember mapUnpack
                                        )
                                    )
                                    rec0.attributes
                                , Just ( "id", Json.Encode.string rec0.id )
                                , Maybe.map
                                    (\mapUnpack ->
                                        ( "relationships"
                                        , Basics.identity mapUnpack
                                        )
                                    )
                                    rec0.relationships
                                , Just
                                    ( "type"
                                    , Json.Encode.string rec0.type_
                                    )
                                ]
                            )
                    )
                    rec.data
                )
            , Maybe.map
                (\mapUnpack ->
                    ( "included", Json.Encode.list Basics.identity mapUnpack )
                )
                rec.included
            , Just ( "meta", encodeJSONAPIResponseMeta rec.meta )
            ]
        )


encodePledgeEvent : PatreonApi.Types.PledgeEvent -> Json.Encode.Value
encodePledgeEvent rec =
    Json.Encode.object
        (List.filterMap
            Basics.identity
            [ Maybe.map
                (\mapUnpack -> ( "amount_cents", Json.Encode.float mapUnpack ))
                rec.amount_cents
            , Maybe.map
                (\mapUnpack ->
                    ( "currency_code", Json.Encode.string mapUnpack )
                )
                rec.currency_code
            , Maybe.map
                (\mapUnpack ->
                    ( "date", OpenApi.Common.encodeStringDateTime mapUnpack )
                )
                rec.date
            , Maybe.map
                (\mapUnpack ->
                    ( "payment_status", Json.Encode.string mapUnpack )
                )
                rec.payment_status
            , Maybe.map
                (\mapUnpack ->
                    ( "pledge_payment_status", Json.Encode.string mapUnpack )
                )
                rec.pledge_payment_status
            , Maybe.map
                (\mapUnpack ->
                    ( "tier_id"
                    , case mapUnpack of
                        OpenApi.Common.Null ->
                            Json.Encode.null

                        OpenApi.Common.Present value ->
                            Json.Encode.string value
                    )
                )
                rec.tier_id
            , Maybe.map
                (\mapUnpack ->
                    ( "tier_title"
                    , case mapUnpack of
                        OpenApi.Common.Null ->
                            Json.Encode.null

                        OpenApi.Common.Present value ->
                            Json.Encode.string value
                    )
                )
                rec.tier_title
            , Maybe.map
                (\mapUnpack -> ( "type", Json.Encode.string mapUnpack ))
                rec.type_
            ]
        )


encodePost : PatreonApi.Types.Post -> Json.Encode.Value
encodePost rec =
    Json.Encode.object
        (List.filterMap
            Basics.identity
            [ Maybe.map
                (\mapUnpack ->
                    ( "app_id"
                    , case mapUnpack of
                        OpenApi.Common.Null ->
                            Json.Encode.null

                        OpenApi.Common.Present value ->
                            Json.Encode.float value
                    )
                )
                rec.app_id
            , Maybe.map
                (\mapUnpack ->
                    ( "app_status"
                    , case mapUnpack of
                        OpenApi.Common.Null ->
                            Json.Encode.null

                        OpenApi.Common.Present value ->
                            Json.Encode.string value
                    )
                )
                rec.app_status
            , Maybe.map
                (\mapUnpack ->
                    ( "content"
                    , case mapUnpack of
                        OpenApi.Common.Null ->
                            Json.Encode.null

                        OpenApi.Common.Present value ->
                            Json.Encode.string value
                    )
                )
                rec.content
            , Maybe.map
                (\mapUnpack ->
                    ( "embed_data"
                    , case mapUnpack of
                        OpenApi.Common.Null ->
                            Json.Encode.null

                        OpenApi.Common.Present value ->
                            Json.Encode.object []
                    )
                )
                rec.embed_data
            , Maybe.map
                (\mapUnpack ->
                    ( "embed_url"
                    , case mapUnpack of
                        OpenApi.Common.Null ->
                            Json.Encode.null

                        OpenApi.Common.Present value ->
                            OpenApi.Common.encodeStringUri value
                    )
                )
                rec.embed_url
            , Maybe.map
                (\mapUnpack ->
                    ( "is_paid"
                    , case mapUnpack of
                        OpenApi.Common.Null ->
                            Json.Encode.null

                        OpenApi.Common.Present value ->
                            Json.Encode.bool value
                    )
                )
                rec.is_paid
            , Maybe.map
                (\mapUnpack ->
                    ( "is_public"
                    , case mapUnpack of
                        OpenApi.Common.Null ->
                            Json.Encode.null

                        OpenApi.Common.Present value ->
                            Json.Encode.bool value
                    )
                )
                rec.is_public
            , Maybe.map
                (\mapUnpack ->
                    ( "published_at"
                    , case mapUnpack of
                        OpenApi.Common.Null ->
                            Json.Encode.null

                        OpenApi.Common.Present value ->
                            OpenApi.Common.encodeStringDateTime value
                    )
                )
                rec.published_at
            , Maybe.map
                (\mapUnpack ->
                    ( "tiers"
                    , case mapUnpack of
                        OpenApi.Common.Null ->
                            Json.Encode.null

                        OpenApi.Common.Present value ->
                            Json.Encode.list Json.Encode.string value
                    )
                )
                rec.tiers
            , Maybe.map
                (\mapUnpack ->
                    ( "title"
                    , case mapUnpack of
                        OpenApi.Common.Null ->
                            Json.Encode.null

                        OpenApi.Common.Present value ->
                            Json.Encode.string value
                    )
                )
                rec.title
            , Maybe.map
                (\mapUnpack ->
                    ( "url", OpenApi.Common.encodeStringUri mapUnpack )
                )
                rec.url
            ]
        )


encodePostResponse : PatreonApi.Types.PostResponse -> Json.Encode.Value
encodePostResponse rec =
    Json.Encode.object
        (List.filterMap
            Basics.identity
            [ Just
                ( "data"
                , Json.Encode.object
                    (List.filterMap
                        Basics.identity
                        [ Maybe.map
                            (\mapUnpack ->
                                ( "attributes", encodePost mapUnpack )
                            )
                            rec.data.attributes
                        , Just ( "id", Json.Encode.string rec.data.id )
                        , Maybe.map
                            (\mapUnpack ->
                                ( "relationships"
                                , Basics.identity mapUnpack
                                )
                            )
                            rec.data.relationships
                        , Just ( "type", Json.Encode.string rec.data.type_ )
                        ]
                    )
                )
            , Maybe.map
                (\mapUnpack ->
                    ( "included", Json.Encode.list Basics.identity mapUnpack )
                )
                rec.included
            , Just ( "links", encodeJSONAPIResponseLinks rec.links )
            ]
        )


encodePostsResponse : PatreonApi.Types.PostsResponse -> Json.Encode.Value
encodePostsResponse rec =
    Json.Encode.object
        (List.filterMap
            Basics.identity
            [ Just
                ( "data"
                , Json.Encode.list
                    (\rec0 ->
                        Json.Encode.object
                            (List.filterMap
                                Basics.identity
                                [ Maybe.map
                                    (\mapUnpack ->
                                        ( "attributes"
                                        , encodePost mapUnpack
                                        )
                                    )
                                    rec0.attributes
                                , Just ( "id", Json.Encode.string rec0.id )
                                , Maybe.map
                                    (\mapUnpack ->
                                        ( "relationships"
                                        , Basics.identity mapUnpack
                                        )
                                    )
                                    rec0.relationships
                                , Just
                                    ( "type"
                                    , Json.Encode.string rec0.type_
                                    )
                                ]
                            )
                    )
                    rec.data
                )
            , Maybe.map
                (\mapUnpack ->
                    ( "included", Json.Encode.list Basics.identity mapUnpack )
                )
                rec.included
            , Just ( "meta", encodeJSONAPIResponseMeta rec.meta )
            ]
        )


encodeStatusCode400 : PatreonApi.Types.StatusCode400 -> Json.Encode.Value
encodeStatusCode400 =
    Json.Encode.list encodeJSONAPIError


encodeStatusCode401 : PatreonApi.Types.StatusCode401 -> Json.Encode.Value
encodeStatusCode401 =
    Json.Encode.list encodeJSONAPIError


encodeStatusCode403 : PatreonApi.Types.StatusCode403 -> Json.Encode.Value
encodeStatusCode403 =
    Json.Encode.list encodeJSONAPIError


encodeStatusCode404 : PatreonApi.Types.StatusCode404 -> Json.Encode.Value
encodeStatusCode404 =
    Json.Encode.list encodeJSONAPIError


encodeStatusCode405 : PatreonApi.Types.StatusCode405 -> Json.Encode.Value
encodeStatusCode405 =
    Json.Encode.list encodeJSONAPIError


encodeStatusCode406 : PatreonApi.Types.StatusCode406 -> Json.Encode.Value
encodeStatusCode406 =
    Json.Encode.list encodeJSONAPIError


encodeStatusCode410 : PatreonApi.Types.StatusCode410 -> Json.Encode.Value
encodeStatusCode410 =
    Json.Encode.list encodeJSONAPIError


encodeStatusCode429 : PatreonApi.Types.StatusCode429 -> Json.Encode.Value
encodeStatusCode429 =
    Json.Encode.list encodeJSONAPIError


encodeStatusCode500 : PatreonApi.Types.StatusCode500 -> Json.Encode.Value
encodeStatusCode500 =
    Json.Encode.list encodeJSONAPIError


encodeStatusCode503 : PatreonApi.Types.StatusCode503 -> Json.Encode.Value
encodeStatusCode503 =
    Json.Encode.list encodeJSONAPIError


encodeTier : PatreonApi.Types.Tier -> Json.Encode.Value
encodeTier rec =
    Json.Encode.object
        (List.filterMap
            Basics.identity
            [ Maybe.map
                (\mapUnpack -> ( "amount_cents", Json.Encode.float mapUnpack ))
                rec.amount_cents
            , Maybe.map
                (\mapUnpack ->
                    ( "created_at"
                    , OpenApi.Common.encodeStringDateTime mapUnpack
                    )
                )
                rec.created_at
            , Maybe.map
                (\mapUnpack -> ( "description", Json.Encode.string mapUnpack ))
                rec.description
            , Maybe.map
                (\mapUnpack ->
                    ( "discord_role_ids"
                    , case mapUnpack of
                        OpenApi.Common.Null ->
                            Json.Encode.null

                        OpenApi.Common.Present value ->
                            Json.Encode.list Json.Encode.string value
                    )
                )
                rec.discord_role_ids
            , Maybe.map
                (\mapUnpack ->
                    ( "edited_at"
                    , OpenApi.Common.encodeStringDateTime mapUnpack
                    )
                )
                rec.edited_at
            , Maybe.map
                (\mapUnpack ->
                    ( "image_url"
                    , case mapUnpack of
                        OpenApi.Common.Null ->
                            Json.Encode.null

                        OpenApi.Common.Present value ->
                            OpenApi.Common.encodeStringUri value
                    )
                )
                rec.image_url
            , Maybe.map
                (\mapUnpack -> ( "patron_count", Json.Encode.float mapUnpack ))
                rec.patron_count
            , Maybe.map
                (\mapUnpack -> ( "post_count", Json.Encode.float mapUnpack ))
                rec.post_count
            , Maybe.map
                (\mapUnpack -> ( "published", Json.Encode.bool mapUnpack ))
                rec.published
            , Maybe.map
                (\mapUnpack ->
                    ( "published_at"
                    , case mapUnpack of
                        OpenApi.Common.Null ->
                            Json.Encode.null

                        OpenApi.Common.Present value ->
                            OpenApi.Common.encodeStringDateTime value
                    )
                )
                rec.published_at
            , Maybe.map
                (\mapUnpack ->
                    ( "remaining"
                    , case mapUnpack of
                        OpenApi.Common.Null ->
                            Json.Encode.null

                        OpenApi.Common.Present value ->
                            Json.Encode.float value
                    )
                )
                rec.remaining
            , Maybe.map
                (\mapUnpack ->
                    ( "requires_shipping", Json.Encode.bool mapUnpack )
                )
                rec.requires_shipping
            , Maybe.map
                (\mapUnpack -> ( "title", Json.Encode.string mapUnpack ))
                rec.title
            , Maybe.map
                (\mapUnpack ->
                    ( "unpublished_at"
                    , case mapUnpack of
                        OpenApi.Common.Null ->
                            Json.Encode.null

                        OpenApi.Common.Present value ->
                            OpenApi.Common.encodeStringDateTime value
                    )
                )
                rec.unpublished_at
            , Maybe.map
                (\mapUnpack ->
                    ( "url", OpenApi.Common.encodeStringUri mapUnpack )
                )
                rec.url
            , Maybe.map
                (\mapUnpack ->
                    ( "user_limit"
                    , case mapUnpack of
                        OpenApi.Common.Null ->
                            Json.Encode.null

                        OpenApi.Common.Present value ->
                            Json.Encode.float value
                    )
                )
                rec.user_limit
            ]
        )


encodeUser : PatreonApi.Types.User -> Json.Encode.Value
encodeUser rec =
    Json.Encode.object
        (List.filterMap
            Basics.identity
            [ Maybe.map
                (\mapUnpack ->
                    ( "about"
                    , case mapUnpack of
                        OpenApi.Common.Null ->
                            Json.Encode.null

                        OpenApi.Common.Present value ->
                            Json.Encode.string value
                    )
                )
                rec.about
            , Maybe.map
                (\mapUnpack ->
                    ( "can_see_nsfw"
                    , case mapUnpack of
                        OpenApi.Common.Null ->
                            Json.Encode.null

                        OpenApi.Common.Present value ->
                            Json.Encode.bool value
                    )
                )
                rec.can_see_nsfw
            , Maybe.map
                (\mapUnpack ->
                    ( "created", OpenApi.Common.encodeStringDateTime mapUnpack )
                )
                rec.created
            , Maybe.map
                (\mapUnpack -> ( "email", Json.Encode.string mapUnpack ))
                rec.email
            , Maybe.map
                (\mapUnpack ->
                    ( "first_name"
                    , case mapUnpack of
                        OpenApi.Common.Null ->
                            Json.Encode.null

                        OpenApi.Common.Present value ->
                            Json.Encode.string value
                    )
                )
                rec.first_name
            , Maybe.map
                (\mapUnpack -> ( "full_name", Json.Encode.string mapUnpack ))
                rec.full_name
            , Maybe.map
                (\mapUnpack ->
                    ( "hide_pledges"
                    , case mapUnpack of
                        OpenApi.Common.Null ->
                            Json.Encode.null

                        OpenApi.Common.Present value ->
                            Json.Encode.bool value
                    )
                )
                rec.hide_pledges
            , Maybe.map
                (\mapUnpack ->
                    ( "image_url", OpenApi.Common.encodeStringUri mapUnpack )
                )
                rec.image_url
            , Maybe.map
                (\mapUnpack -> ( "is_creator", Json.Encode.bool mapUnpack ))
                rec.is_creator
            , Maybe.map
                (\mapUnpack ->
                    ( "is_email_verified", Json.Encode.bool mapUnpack )
                )
                rec.is_email_verified
            , Maybe.map
                (\mapUnpack ->
                    ( "last_name"
                    , case mapUnpack of
                        OpenApi.Common.Null ->
                            Json.Encode.null

                        OpenApi.Common.Present value ->
                            Json.Encode.string value
                    )
                )
                rec.last_name
            , Maybe.map
                (\mapUnpack -> ( "like_count", Json.Encode.float mapUnpack ))
                rec.like_count
            , Maybe.map
                (\mapUnpack -> ( "social_connections", Json.Encode.object [] ))
                rec.social_connections
            , Maybe.map
                (\mapUnpack ->
                    ( "thumb_url", OpenApi.Common.encodeStringUri mapUnpack )
                )
                rec.thumb_url
            , Maybe.map
                (\mapUnpack ->
                    ( "url", OpenApi.Common.encodeStringUri mapUnpack )
                )
                rec.url
            , Maybe.map
                (\mapUnpack ->
                    ( "vanity"
                    , case mapUnpack of
                        OpenApi.Common.Null ->
                            Json.Encode.null

                        OpenApi.Common.Present value ->
                            Json.Encode.string value
                    )
                )
                rec.vanity
            ]
        )


encodeUserResponse : PatreonApi.Types.UserResponse -> Json.Encode.Value
encodeUserResponse rec =
    Json.Encode.object
        (List.filterMap
            Basics.identity
            [ Just
                ( "data"
                , Json.Encode.object
                    (List.filterMap
                        Basics.identity
                        [ Maybe.map
                            (\mapUnpack ->
                                ( "attributes", encodeUser mapUnpack )
                            )
                            rec.data.attributes
                        , Just ( "id", Json.Encode.string rec.data.id )
                        , Maybe.map
                            (\mapUnpack ->
                                ( "relationships"
                                , Basics.identity mapUnpack
                                )
                            )
                            rec.data.relationships
                        , Just ( "type", Json.Encode.string rec.data.type_ )
                        ]
                    )
                )
            , Maybe.map
                (\mapUnpack ->
                    ( "included", Json.Encode.list Basics.identity mapUnpack )
                )
                rec.included
            , Just ( "links", encodeJSONAPIResponseLinks rec.links )
            ]
        )


encodeWebhook : PatreonApi.Types.Webhook -> Json.Encode.Value
encodeWebhook rec =
    Json.Encode.object
        (List.filterMap
            Basics.identity
            [ Maybe.map
                (\mapUnpack ->
                    ( "last_attempted_at"
                    , OpenApi.Common.encodeStringDateTime mapUnpack
                    )
                )
                rec.last_attempted_at
            , Maybe.map
                (\mapUnpack ->
                    ( "num_consecutive_times_failed"
                    , Json.Encode.float mapUnpack
                    )
                )
                rec.num_consecutive_times_failed
            , Maybe.map
                (\mapUnpack -> ( "paused", Json.Encode.bool mapUnpack ))
                rec.paused
            , Maybe.map
                (\mapUnpack -> ( "secret", Json.Encode.string mapUnpack ))
                rec.secret
            , Maybe.map
                (\mapUnpack ->
                    ( "triggers"
                    , Json.Encode.list Json.Encode.string mapUnpack
                    )
                )
                rec.triggers
            , Maybe.map
                (\mapUnpack ->
                    ( "uri", OpenApi.Common.encodeStringUri mapUnpack )
                )
                rec.uri
            ]
        )


encodeWebhookResponse : PatreonApi.Types.WebhookResponse -> Json.Encode.Value
encodeWebhookResponse rec =
    Json.Encode.object
        (List.filterMap
            Basics.identity
            [ Just
                ( "data"
                , Json.Encode.object
                    (List.filterMap
                        Basics.identity
                        [ Maybe.map
                            (\mapUnpack ->
                                ( "attributes", encodeWebhook mapUnpack )
                            )
                            rec.data.attributes
                        , Just ( "id", Json.Encode.string rec.data.id )
                        , Maybe.map
                            (\mapUnpack ->
                                ( "relationships"
                                , Basics.identity mapUnpack
                                )
                            )
                            rec.data.relationships
                        , Just ( "type", Json.Encode.string rec.data.type_ )
                        ]
                    )
                )
            , Maybe.map
                (\mapUnpack ->
                    ( "included", Json.Encode.list Basics.identity mapUnpack )
                )
                rec.included
            , Just ( "links", encodeJSONAPIResponseLinks rec.links )
            ]
        )


encodeWebhookTrigger : PatreonApi.Types.WebhookTrigger -> Json.Encode.Value
encodeWebhookTrigger =
    Json.Encode.list Json.Encode.string


encodeWebhooksResponse : PatreonApi.Types.WebhooksResponse -> Json.Encode.Value
encodeWebhooksResponse rec =
    Json.Encode.object
        (List.filterMap
            Basics.identity
            [ Just
                ( "data"
                , Json.Encode.list
                    (\rec0 ->
                        Json.Encode.object
                            (List.filterMap
                                Basics.identity
                                [ Maybe.map
                                    (\mapUnpack ->
                                        ( "attributes"
                                        , encodeWebhook mapUnpack
                                        )
                                    )
                                    rec0.attributes
                                , Just ( "id", Json.Encode.string rec0.id )
                                , Maybe.map
                                    (\mapUnpack ->
                                        ( "relationships"
                                        , Basics.identity mapUnpack
                                        )
                                    )
                                    rec0.relationships
                                , Just
                                    ( "type"
                                    , Json.Encode.string rec0.type_
                                    )
                                ]
                            )
                    )
                    rec.data
                )
            , Maybe.map
                (\mapUnpack ->
                    ( "included", Json.Encode.list Basics.identity mapUnpack )
                )
                rec.included
            , Just ( "meta", encodeJSONAPIResponseMeta rec.meta )
            ]
        )
