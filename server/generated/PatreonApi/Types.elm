module PatreonApi.Types exposing
    ( Address, Benefit, Campaign, CampaignResponse, CampaignsResponse, Client, Deliverable, Goal, JSONAPIError
    , JSONAPILinksRelated, JSONAPIResource, JSONAPIResponseLinks, JSONAPIResponseMeta, Media, Member
    , MemberResponse, MembersResponse, PledgeEvent, Post, PostResponse, PostsResponse, StatusCode400
    , StatusCode401, StatusCode403, StatusCode404, StatusCode405, StatusCode406, StatusCode410, StatusCode429
    , StatusCode500, StatusCode503, Tier, User, UserResponse, Webhook, WebhookResponse, WebhookTrigger
    , WebhooksResponse
    , CreateWebhook_Error(..), EditWebhook_Error(..), GetCampaignMembers_Error(..), GetCampaignPosts_Error(..)
    , GetCampaign_Error(..), GetCampaigns_Error(..), GetIdentity_Error(..), GetMember_Error(..), GetPost_Error(..)
    , GetWebhooks_Error(..)
    )

{-|


## Aliases

@docs Address, Benefit, Campaign, CampaignResponse, CampaignsResponse, Client, Deliverable, Goal, JSONAPIError
@docs JSONAPILinksRelated, JSONAPIResource, JSONAPIResponseLinks, JSONAPIResponseMeta, Media, Member
@docs MemberResponse, MembersResponse, PledgeEvent, Post, PostResponse, PostsResponse, StatusCode400
@docs StatusCode401, StatusCode403, StatusCode404, StatusCode405, StatusCode406, StatusCode410, StatusCode429
@docs StatusCode500, StatusCode503, Tier, User, UserResponse, Webhook, WebhookResponse, WebhookTrigger
@docs WebhooksResponse


## Errors

@docs CreateWebhook_Error, EditWebhook_Error, GetCampaignMembers_Error, GetCampaignPosts_Error
@docs GetCampaign_Error, GetCampaigns_Error, GetIdentity_Error, GetMember_Error, GetPost_Error
@docs GetWebhooks_Error

-}

import Json.Encode
import OpenApi.Common
import Time
import Url


{-| A patron's shipping address.

  - addressee:
    Full recipient name

  - city:

  - country:

  - created\_at:
    Datetime address was first created

  - line\_1:
    First line of street address

  - line\_2:
    Second line of street address

  - phone\_number:
    Telephone number. Specified for non-US addresses

  - postal\_code:
    Postal or zip code

  - state:
    State or province name

-}
type alias Address =
    { addressee : Maybe (OpenApi.Common.Nullable String)
    , city : Maybe String
    , country : Maybe String
    , created_at : Maybe Time.Posix
    , line_1 : Maybe (OpenApi.Common.Nullable String)
    , line_2 : Maybe (OpenApi.Common.Nullable String)
    , phone_number : Maybe (OpenApi.Common.Nullable String)
    , postal_code : Maybe (OpenApi.Common.Nullable String)
    , state : Maybe (OpenApi.Common.Nullable String)
    }


{-| A benefit added to the campaign, which can be added to a tier to be delivered to the patron.

  - app\_external\_id:
    The third-party external ID this reward is associated with
  - app\_meta:
    Any metadata the third-party app included with this benefit on creation
  - benefit\_type:
    Type of benefit, such as `custom` for creator-defined benefits
  - created\_at:
    Datetime this benefit was created
  - deliverables\_due\_today\_count:
    Number of deliverables for this benefit that are due today specifically
  - delivered\_deliverables\_count:
    Number of deliverables for this benefit that have been marked complete
  - description:
    Display description
  - is\_deleted:
    Whether this benefit has been deleted
  - is\_ended:
    Whether this benefit is no longer available to new patrons
  - is\_published:
    Whether this benefit is ready to be fulfilled to patrons
  - next\_deliverable\_due\_date:
    The next due date (after EOD today) for this benefit
  - not\_delivered\_deliverables\_count:
    Number of deliverables for this benefit that are due, for all dates
  - rule\_type:
    A rule type designation, such as `eom_monthly` or `one_time_immediate`
  - tiers\_count:
    Number of tiers containing this benefit
  - title:
    Display title

-}
type alias Benefit =
    { app_external_id : Maybe (OpenApi.Common.Nullable String)
    , app_meta : Maybe (OpenApi.Common.Nullable {})
    , benefit_type : Maybe (OpenApi.Common.Nullable String)
    , created_at : Maybe Time.Posix
    , deliverables_due_today_count : Maybe Float
    , delivered_deliverables_count : Maybe Float
    , description : Maybe (OpenApi.Common.Nullable String)
    , is_deleted : Maybe Bool
    , is_ended : Maybe Bool
    , is_published : Maybe Bool
    , next_deliverable_due_date : Maybe (OpenApi.Common.Nullable Time.Posix)
    , not_delivered_deliverables_count : Maybe Float
    , rule_type : Maybe (OpenApi.Common.Nullable String)
    , tiers_count : Maybe Float
    , title : Maybe String
    }


{-| The creator's page, and the top-level object for accessing lists of members, tiers, etc

  - created\_at:
    Datetime that the creator first began the campaign creation process

  - creation\_name:
    The type of content the creator is creating, as in "{@link Campaign.vanity} is creating {@link Campaign.creation\_name}"

  - discord\_server\_id:
    The ID of the external discord server that is linked to this campaign

  - google\_analytics\_id:
    The ID of the Google Analytics tracker that the creator wants metrics to be sent to

  - has\_rss:
    Whether this user has opted-in to rss feeds

  - has\_sent\_rss\_notify:
    Whether the creator has sent a one-time rss notification email

  - image\_small\_url:
    Profile image URL for the campaign

  - image\_url:
    Banner image URL for the campaign

  - is\_charged\_immediately:
    Whether the campaign charges upfront

  - is\_monthly:
    Whether the campaign charges per month

  - is\_nsfw:
    Whether the creator has marked the campaign as containing nsfw content

  - main\_video\_embed:

  - main\_video\_url:

  - one\_liner:
    Pithy one-liner for this campaign, displayed on the creator page

  - patron\_count:
    Number of patrons having access to this creator

  - pay\_per\_name:
    The thing which patrons are paying per, as in "{@link Campaign.vanity} is making $1000 per {@link Campaign.pay\_per\_name}"

  - pledge\_url:
    Relative (to patreon.com) URL for the pledge checkout flow for this campaign

  - published\_at:
    Datetime that the creator most recently published (made publicly visible) the campaign.
    Null when the campaign has not been public.

  - rss\_artwork\_url:
    The url for the rss album artwork

  - rss\_feed\_title:
    The title of the campaigns rss feed

  - show\_earnings:
    Whether the campaign's total earnings are shown publicly

  - summary:
    The summary of this campaign.
    Can be viewed in the About section of the campaign

  - thanks\_embed:
    The embed that is shown to patrons after pledging

  - thanks\_msg:
    The message that is shown to patrons after pledging

  - thanks\_video\_url:
    The URL of the video that is shown to patrons after pledging

  - url:
    The url to visit this campaign

  - vanity:
    The campaign's vanity name

-}
type alias Campaign =
    { created_at : Maybe Time.Posix
    , creation_name : Maybe (OpenApi.Common.Nullable String)
    , discord_server_id : Maybe (OpenApi.Common.Nullable String)
    , google_analytics_id : Maybe (OpenApi.Common.Nullable String)
    , has_rss : Maybe Bool
    , has_sent_rss_notify : Maybe Bool
    , image_small_url : Maybe Url.Url
    , image_url : Maybe Url.Url
    , is_charged_immediately : Maybe Bool
    , is_monthly : Maybe Bool
    , is_nsfw : Maybe Bool
    , main_video_embed : Maybe (OpenApi.Common.Nullable String)
    , main_video_url : Maybe (OpenApi.Common.Nullable Url.Url)
    , one_liner : Maybe (OpenApi.Common.Nullable String)
    , patron_count : Maybe Float
    , pay_per_name : Maybe (OpenApi.Common.Nullable String)
    , pledge_url : Maybe String
    , published_at : Maybe (OpenApi.Common.Nullable Time.Posix)
    , rss_artwork_url : Maybe (OpenApi.Common.Nullable Url.Url)
    , rss_feed_title : Maybe (OpenApi.Common.Nullable String)
    , show_earnings : Maybe Bool
    , summary : Maybe (OpenApi.Common.Nullable String)
    , thanks_embed : Maybe (OpenApi.Common.Nullable String)
    , thanks_msg : Maybe (OpenApi.Common.Nullable String)
    , thanks_video_url : Maybe (OpenApi.Common.Nullable Url.Url)
    , url : Maybe Url.Url
    , vanity : Maybe (OpenApi.Common.Nullable String)
    }


type alias CampaignResponse =
    { data :
        { attributes : Maybe Campaign
        , id : String
        , relationships : Maybe Json.Encode.Value
        , type_ : String
        }
    , included : Maybe (List Json.Encode.Value)
    , links : JSONAPIResponseLinks
    }


type alias CampaignsResponse =
    { data :
        List
            { attributes : Maybe Campaign
            , id : String
            , relationships : Maybe Json.Encode.Value
            , type_ : String
            }
    , included : Maybe (List Json.Encode.Value)
    , meta : JSONAPIResponseMeta
    }


{-| A client created by a developer, used for getting OAuth2 access tokens.

  - author\_name:
    The author name provided during client setup.

  - category:

  - client\_secret:
    The client's secret.

  - default\_scopes:
    The client's default OAuth scopes for the authorization flow.

  - description:
    The description provided during client setup.

  - domain:
    The domain provided during client setup.

  - icon\_url:
    The URL of the icon used in the OAuth authorization flow.

  - name:
    The name provided during client setup.

  - privacy\_policy\_url:
    The URL of the privacy policy provided during client setup.

  - redirect\_uris:
    The allowable redirect URIs for the OAuth authorization flow.

  - tos\_url:
    The URL of the terms of service provided during client setup.

  - version:
    The Patreon API version the client is targeting.

-}
type alias Client =
    { author_name : Maybe (OpenApi.Common.Nullable String)
    , category : Maybe String
    , client_secret : Maybe String
    , default_scopes : Maybe String
    , description : Maybe String
    , domain : Maybe (OpenApi.Common.Nullable Url.Url)
    , icon_url : Maybe (OpenApi.Common.Nullable Url.Url)
    , name : Maybe String
    , privacy_policy_url : Maybe (OpenApi.Common.Nullable Url.Url)
    , redirect_uris : Maybe String
    , tos_url : Maybe Url.Url
    , version : Maybe Float
    }


type CreateWebhook_Error
    = CreateWebhook_400 StatusCode400
    | CreateWebhook_401 StatusCode401
    | CreateWebhook_403 StatusCode403
    | CreateWebhook_404 StatusCode404
    | CreateWebhook_405 StatusCode405
    | CreateWebhook_406 StatusCode406
    | CreateWebhook_410 StatusCode410
    | CreateWebhook_429 StatusCode429
    | CreateWebhook_500 StatusCode500
    | CreateWebhook_503 StatusCode503


{-| The record of whether or not a patron has been delivered the benefit they are owed because of their member tier.

  - completed\_at:
    When the creator marked the deliverable as completed or fulfilled to the patron
  - delivery\_status:
  - due\_at:
    When the deliverable is due to the patron

-}
type alias Deliverable =
    { completed_at : Maybe (OpenApi.Common.Nullable Time.Posix)
    , delivery_status : Maybe String
    , due_at : Maybe Time.Posix
    }


type EditWebhook_Error
    = EditWebhook_400 StatusCode400
    | EditWebhook_401 StatusCode401
    | EditWebhook_403 StatusCode403
    | EditWebhook_404 StatusCode404
    | EditWebhook_405 StatusCode405
    | EditWebhook_406 StatusCode406
    | EditWebhook_410 StatusCode410
    | EditWebhook_429 StatusCode429
    | EditWebhook_500 StatusCode500
    | EditWebhook_503 StatusCode503


type GetCampaignMembers_Error
    = GetCampaignMembers_400 StatusCode400
    | GetCampaignMembers_401 StatusCode401
    | GetCampaignMembers_403 StatusCode403
    | GetCampaignMembers_404 StatusCode404
    | GetCampaignMembers_405 StatusCode405
    | GetCampaignMembers_406 StatusCode406
    | GetCampaignMembers_410 StatusCode410
    | GetCampaignMembers_429 StatusCode429
    | GetCampaignMembers_500 StatusCode500
    | GetCampaignMembers_503 StatusCode503


type GetCampaignPosts_Error
    = GetCampaignPosts_400 StatusCode400
    | GetCampaignPosts_401 StatusCode401
    | GetCampaignPosts_403 StatusCode403
    | GetCampaignPosts_404 StatusCode404
    | GetCampaignPosts_405 StatusCode405
    | GetCampaignPosts_406 StatusCode406
    | GetCampaignPosts_410 StatusCode410
    | GetCampaignPosts_429 StatusCode429
    | GetCampaignPosts_500 StatusCode500
    | GetCampaignPosts_503 StatusCode503


type GetCampaign_Error
    = GetCampaign_400 StatusCode400
    | GetCampaign_401 StatusCode401
    | GetCampaign_403 StatusCode403
    | GetCampaign_404 StatusCode404
    | GetCampaign_405 StatusCode405
    | GetCampaign_406 StatusCode406
    | GetCampaign_410 StatusCode410
    | GetCampaign_429 StatusCode429
    | GetCampaign_500 StatusCode500
    | GetCampaign_503 StatusCode503


type GetCampaigns_Error
    = GetCampaigns_400 StatusCode400
    | GetCampaigns_401 StatusCode401
    | GetCampaigns_403 StatusCode403
    | GetCampaigns_404 StatusCode404
    | GetCampaigns_405 StatusCode405
    | GetCampaigns_406 StatusCode406
    | GetCampaigns_410 StatusCode410
    | GetCampaigns_429 StatusCode429
    | GetCampaigns_500 StatusCode500
    | GetCampaigns_503 StatusCode503


type GetIdentity_Error
    = GetIdentity_400 StatusCode400
    | GetIdentity_401 StatusCode401
    | GetIdentity_403 StatusCode403
    | GetIdentity_404 StatusCode404
    | GetIdentity_405 StatusCode405
    | GetIdentity_406 StatusCode406
    | GetIdentity_410 StatusCode410
    | GetIdentity_429 StatusCode429
    | GetIdentity_500 StatusCode500
    | GetIdentity_503 StatusCode503


type GetMember_Error
    = GetMember_400 StatusCode400
    | GetMember_401 StatusCode401
    | GetMember_403 StatusCode403
    | GetMember_404 StatusCode404
    | GetMember_405 StatusCode405
    | GetMember_406 StatusCode406
    | GetMember_410 StatusCode410
    | GetMember_429 StatusCode429
    | GetMember_500 StatusCode500
    | GetMember_503 StatusCode503


type GetPost_Error
    = GetPost_400 StatusCode400
    | GetPost_401 StatusCode401
    | GetPost_403 StatusCode403
    | GetPost_404 StatusCode404
    | GetPost_405 StatusCode405
    | GetPost_406 StatusCode406
    | GetPost_410 StatusCode410
    | GetPost_429 StatusCode429
    | GetPost_500 StatusCode500
    | GetPost_503 StatusCode503


type GetWebhooks_Error
    = GetWebhooks_400 StatusCode400
    | GetWebhooks_401 StatusCode401
    | GetWebhooks_403 StatusCode403
    | GetWebhooks_404 StatusCode404
    | GetWebhooks_405 StatusCode405
    | GetWebhooks_406 StatusCode406
    | GetWebhooks_410 StatusCode410
    | GetWebhooks_429 StatusCode429
    | GetWebhooks_500 StatusCode500
    | GetWebhooks_503 StatusCode503


type alias Goal =
    {}


type alias JSONAPIError =
    { code : Maybe Float
    , code_name : Maybe String
    , detail : Maybe String
    , id : Maybe String
    , status : Maybe String
    , title : Maybe String
    }


type alias JSONAPILinksRelated =
    Json.Encode.Value


type alias JSONAPIResource =
    { id : String, type_ : String }


type alias JSONAPIResponseLinks =
    Json.Encode.Value


type alias JSONAPIResponseMeta =
    Json.Encode.Value


{-| A file uploaded to patreon.com, usually an image.

  - created\_at:
    When the file was created

  - download\_url:
    The URL to download this media. Valid for 24 hours.

  - file\_name:
    File name.

  - image\_urls:
    The resized image URLs for this media. Valid for 2 weeks.

  - metadata:
    Metadata related to the file

  - mimetype:
    Mimetype of uploaded file, eg: "application/jpeg"

  - owner\_id:
    Ownership id (See also {@link Media.owner\_type})

  - owner\_relationship:
    Ownership relationship type for multi-relationship medias

  - owner\_type:
    Type of the resource that owns the file

  - size\_bytes:

  - state:

  - upload\_expires\_at:
    When the upload URL expires

  - upload\_parameters:
    All the parameters that have to be added to the upload form request

  - upload\_url:
    The URL to perform a POST request to in order to upload the media file

-}
type alias Media =
    { created_at : Maybe Time.Posix
    , download_url : Maybe Url.Url
    , file_name : Maybe String
    , image_urls : Maybe {}
    , metadata : Maybe (OpenApi.Common.Nullable {})
    , mimetype : Maybe String
    , owner_id : Maybe String
    , owner_relationship : Maybe String
    , owner_type : Maybe String
    , size_bytes : Maybe Float
    , state : Maybe String
    , upload_expires_at : Maybe Time.Posix
    , upload_parameters : Maybe {}
    , upload_url : Maybe Url.Url
    }


{-| The record of a user's membership to a campaign.
Remains consistent across months of pledging.

  - campaign\_lifetime\_support\_cents:
    The total amount that the member has ever paid to the campaign in the campaign's currency.
    0 if never paid

  - currently\_entitled\_amount\_cents:
    The amount in cents that the member is entitled to.
    This includes a current pledge, or payment that covers the current payment period

  - email:
    The member's email address.
    Requires the campaigns.members[email] scope

  - full\_name:
    Full name of the member user

  - is\_follower:
    The user is not a pledging patron but has subscribed to updates about public posts.
    This will always be false, following has been replaced by free membership.

  - is\_free\_trial:
    The user is in a free trial period.

  - is\_gifted:
    The user's membership is from a free gift

  - last\_charge\_date:
    Datetime of last attempted charge.
    `null` if never charged

  - last\_charge\_status:
    The result of the last attempted charge.
    The only successful status is `'Paid'`.
    `null` if never charged

    Note: this will likely be either `'Paid'` or `'Pending'`

  - lifetime\_support\_cents:
    The total amount that the member has ever paid to the campaign in the campaign's currency.
    0 if never paid.

    Use {@link campaign\_lifetime\_support\_cents}.

  - next\_charge\_date:
    Datetime of next charge.
    `null` if annual pledge downgrade

  - note:
    The creator's notes on the member

  - patron\_status:
    A `null` value indicates the member has never pledged

  - pledge\_cadence:
    Number of months between charges

    Note: this will be `1` if Campaign.is\_monthly is `true`

  - pledge\_relationship\_start:
    Datetime of beginning of most recent pledge chainfrom this member to the campaign.
    Pledge updates do not change this value

  - will\_pay\_amount\_cents:
    The amount in cents the user will pay at the next pay cycle

-}
type alias Member =
    { campaign_lifetime_support_cents : Maybe Float
    , currently_entitled_amount_cents : Maybe Float
    , email : Maybe String
    , full_name : Maybe String
    , is_follower : Maybe Bool
    , is_free_trial : Maybe Bool
    , is_gifted : Maybe Bool
    , last_charge_date : Maybe (OpenApi.Common.Nullable Time.Posix)
    , last_charge_status : Maybe (OpenApi.Common.Nullable String)
    , lifetime_support_cents : Maybe Float
    , next_charge_date : Maybe (OpenApi.Common.Nullable Time.Posix)
    , note : Maybe String
    , patron_status : Maybe (OpenApi.Common.Nullable String)
    , pledge_cadence : Maybe (OpenApi.Common.Nullable Float)
    , pledge_relationship_start : Maybe (OpenApi.Common.Nullable Time.Posix)
    , will_pay_amount_cents : Maybe Float
    }


type alias MemberResponse =
    { data :
        { attributes : Maybe Member
        , id : String
        , relationships : Maybe Json.Encode.Value
        , type_ : String
        }
    , included : Maybe (List Json.Encode.Value)
    , links : JSONAPIResponseLinks
    }


type alias MembersResponse =
    { data :
        List
            { attributes : Maybe Member
            , id : String
            , relationships : Maybe Json.Encode.Value
            , type_ : String
            }
    , included : Maybe (List Json.Encode.Value)
    , meta : JSONAPIResponseMeta
    }


{-| The record of a pledging action taken by the user, or that action's failure.

  - amount\_cents:
    Amount (in the currency in which the patron paid) of the underlying event
  - currency\_code:
    ISO code of the currency of the event
  - date:
    The date which this event occurred
  - payment\_status:
    Status of underlying payment
  - pledge\_payment\_status:
    The payment status of the pledge
  - tier\_id:
    Id of the tier associated with the pledge
  - tier\_title:
    Title of the reward tier associated with the pledge
  - type\_:
    Event type.

-}
type alias PledgeEvent =
    { amount_cents : Maybe Float
    , currency_code : Maybe String
    , date : Maybe Time.Posix
    , payment_status : Maybe String
    , pledge_payment_status : Maybe String
    , tier_id : Maybe (OpenApi.Common.Nullable String)
    , tier_title : Maybe (OpenApi.Common.Nullable String)
    , type_ : Maybe String
    }


{-| Content posted by a creator on a campaign page.

  - app\_id:
    Platform app id

  - app\_status:
    Processing status of the post

  - content:

  - embed\_data:
    An object containing embed data if media is embedded in the post

  - embed\_url:
    Embed media url

  - is\_paid:
    Whether the post incurs a bill as part of a pay-per-post campaign

  - is\_public:
      - `true` if the post is viewable by anyone
      - `false` if only patrons (or a subset of patrons) can view

  - published\_at:
    Datetime that the creator most recently published (made publicly visible) the post

  - tiers:
    The tier ids that the post is locked for if only patrons (or a subset of patrons) can view.

  - title:

  - url:
    A URL to access this post on patreon.com

-}
type alias Post =
    { app_id : Maybe (OpenApi.Common.Nullable Float)
    , app_status : Maybe (OpenApi.Common.Nullable String)
    , content : Maybe (OpenApi.Common.Nullable String)
    , embed_data : Maybe (OpenApi.Common.Nullable {})
    , embed_url : Maybe (OpenApi.Common.Nullable Url.Url)
    , is_paid : Maybe (OpenApi.Common.Nullable Bool)
    , is_public : Maybe (OpenApi.Common.Nullable Bool)
    , published_at : Maybe (OpenApi.Common.Nullable Time.Posix)
    , tiers : Maybe (OpenApi.Common.Nullable (List String))
    , title : Maybe (OpenApi.Common.Nullable String)
    , url : Maybe Url.Url
    }


type alias PostResponse =
    { data :
        { attributes : Maybe Post
        , id : String
        , relationships : Maybe Json.Encode.Value
        , type_ : String
        }
    , included : Maybe (List Json.Encode.Value)
    , links : JSONAPIResponseLinks
    }


type alias PostsResponse =
    { data :
        List
            { attributes : Maybe Post
            , id : String
            , relationships : Maybe Json.Encode.Value
            , type_ : String
            }
    , included : Maybe (List Json.Encode.Value)
    , meta : JSONAPIResponseMeta
    }


type alias StatusCode400 =
    List JSONAPIError


type alias StatusCode401 =
    List JSONAPIError


type alias StatusCode403 =
    List JSONAPIError


type alias StatusCode404 =
    List JSONAPIError


type alias StatusCode405 =
    List JSONAPIError


type alias StatusCode406 =
    List JSONAPIError


type alias StatusCode410 =
    List JSONAPIError


type alias StatusCode429 =
    List JSONAPIError


type alias StatusCode500 =
    List JSONAPIError


type alias StatusCode503 =
    List JSONAPIError


{-| A membership level on a campaign, which can have benefits attached to it.

  - amount\_cents:
    Monetary amount associated with this tier (in U.S. cents)
  - created\_at:
    Datetime this tier was created
  - description:
    Tier display description
  - discord\_role\_ids:
    The discord role IDs granted by this tier
  - edited\_at:
    Datetime tier was last modified
  - image\_url:
    Full qualified image URL associated with this tier
  - patron\_count:
    Number of patrons currently registered for this tier
  - post\_count:
    Number of posts published to this tier
  - published:
    Whether the tier is currently published
  - published\_at:
    Datetime this tier was last published
  - remaining:
    Remaining number of patrons who may subscribe, if there is a {@link Tier.user\_limit}
  - requires\_shipping:
    Whether this tier requires a shipping address from patrons
  - title:
    Tier display title
  - unpublished\_at:
    Datetime tier was unpublished, while applicable
  - url:
    Fully qualified URL associated with this tier
  - user\_limit:
    Maximum number of patrons this tier is limited to, if applicable

-}
type alias Tier =
    { amount_cents : Maybe Float
    , created_at : Maybe Time.Posix
    , description : Maybe String
    , discord_role_ids : Maybe (OpenApi.Common.Nullable (List String))
    , edited_at : Maybe Time.Posix
    , image_url : Maybe (OpenApi.Common.Nullable Url.Url)
    , patron_count : Maybe Float
    , post_count : Maybe Float
    , published : Maybe Bool
    , published_at : Maybe (OpenApi.Common.Nullable Time.Posix)
    , remaining : Maybe (OpenApi.Common.Nullable Float)
    , requires_shipping : Maybe Bool
    , title : Maybe String
    , unpublished_at : Maybe (OpenApi.Common.Nullable Time.Posix)
    , url : Maybe Url.Url
    , user_limit : Maybe (OpenApi.Common.Nullable Float)
    }


{-| The Patreon user, which can be both patron and creator.

  - about:
    The user's about text, which appears on their profile

  - can\_see\_nsfw:
    Whether this user can view nsfw content

  - created:
    Datetime of this user's account creation

  - email:
    The user's email address.
    Requires certain scopes to access.
    See the scopes section of the documentation

  - first\_name:
    First name.

  - full\_name:
    Combined first and last name

  - hide\_pledges:
    Whether the user has chosen to keep private which creators they pledge to

  - image\_url:
    The user's profile picture URL, scaled to width 400px

  - is\_creator:
    Whether this user has an active campaign.

  - is\_email\_verified:
    Whether the user has confirmed their email

  - last\_name:
    Last name.

  - like\_count:
    How many posts this user has liked

  - social\_connections:
    Mapping from user's connected app names to external user id on the respective app

    NOTE: since the documentation is `object`, this can change without notice.
    For a more accurate representation use the following type using TypeScript module augmentation:

    ```ts
    import 'patreon-api.ts'

    declare module 'patreon-api.ts' {
       interface CustomTypeOptions {
           social_connections: Record<string, { url: string, user_id: string } | null>
       }
    }
    ```

  - thumb\_url:
    The user's profile picture URL, scaled to a square of size 100x100px

  - url:
    URL of this user's creator or patron profile

  - vanity:
    The public "username" of the user.
    patreon.com/ goes to this user's creator page.
    Non-creator users might not have a vanity.

-}
type alias User =
    { about : Maybe (OpenApi.Common.Nullable String)
    , can_see_nsfw : Maybe (OpenApi.Common.Nullable Bool)
    , created : Maybe Time.Posix
    , email : Maybe String
    , first_name : Maybe (OpenApi.Common.Nullable String)
    , full_name : Maybe String
    , hide_pledges : Maybe (OpenApi.Common.Nullable Bool)
    , image_url : Maybe Url.Url
    , is_creator : Maybe Bool
    , is_email_verified : Maybe Bool
    , last_name : Maybe (OpenApi.Common.Nullable String)
    , like_count : Maybe Float
    , social_connections : Maybe {}
    , thumb_url : Maybe Url.Url
    , url : Maybe Url.Url
    , vanity : Maybe (OpenApi.Common.Nullable String)
    }


type alias UserResponse =
    { data :
        { attributes : Maybe User
        , id : String
        , relationships : Maybe Json.Encode.Value
        , type_ : String
        }
    , included : Maybe (List Json.Encode.Value)
    , links : JSONAPIResponseLinks
    }


{-| Webhooks are fired based on events happening on a particular campaign.

  - last\_attempted\_at:
    Last date that the webhook was attempted or used.
  - num\_consecutive\_times\_failed:
    Number of times the webhook has failed consecutively, when in an error state.
  - paused:
    `true` if the webhook is paused as a result of repeated failed attempts to post to uri.
    Set to `false` to attempt to re-enable a previously failing webhook.
  - secret:
    Secret used to sign your webhook message body, so you can validate authenticity upon receipt.
  - triggers:
    List of events that will trigger this webhook.
  - uri:
    Fully qualified uri where webhook will be sent

-}
type alias Webhook =
    { last_attempted_at : Maybe Time.Posix
    , num_consecutive_times_failed : Maybe Float
    , paused : Maybe Bool
    , secret : Maybe String
    , triggers : Maybe (List String)
    , uri : Maybe Url.Url
    }


type alias WebhookResponse =
    { data :
        { attributes : Maybe Webhook
        , id : String
        , relationships : Maybe Json.Encode.Value
        , type_ : String
        }
    , included : Maybe (List Json.Encode.Value)
    , links : JSONAPIResponseLinks
    }


type alias WebhookTrigger =
    List String


type alias WebhooksResponse =
    { data :
        List
            { attributes : Maybe Webhook
            , id : String
            , relationships : Maybe Json.Encode.Value
            , type_ : String
            }
    , included : Maybe (List Json.Encode.Value)
    , meta : JSONAPIResponseMeta
    }
