module Api exposing (getPosts)

import BackendTask exposing (BackendTask)
import BackendTask.Http as Http
import FatalError exposing (FatalError)
import Json.Decode
import Json.Decode.Pipeline
import Url.Builder


getPosts :
    { a | cookie : String }
    -> BackendTask { fatal : FatalError, recoverable : Http.Error } (List Post)
getPosts cookie =
    getPaginated cookie postsUrl postDecoder
        |> BackendTask.map
            (\_ -> [])


getPaginated :
    { cfg | cookie : String }
    -> (String -> String)
    -> Json.Decode.Decoder a
    -> BackendTask { fatal : FatalError, recoverable : Http.Error } (List a)
getPaginated { cookie } toUrl decoder =
    let
        go :
            String
            -> List (List a)
            -> BackendTask { fatal : FatalError, recoverable : Http.Error } (List a)
        go cursor acc =
            Http.request
                { url = toUrl cursor
                , method = "GET"
                , body = Http.emptyBody
                , headers =
                    [ ( "User-Agent", "Mozilla/5.0 (X11; Linux x86_64; rv:133.0) Gecko/20100101 Firefox/133.0" )
                    , ( "Referer", "https://www.patreon.com/c/orlagartland/posts" )
                    , ( "Cookie", cookie )
                    ]
                , retries = Nothing
                , timeoutInMs = Nothing
                }
                (Http.expectJson
                    (Json.Decode.map2 (\data next -> { data = data, next = next })
                        (Json.Decode.field "data" (Json.Decode.list decoder))
                        (Json.Decode.at
                            [ "meta", "pagination", "cursors", "next" ]
                            (Json.Decode.nullable Json.Decode.string)
                        )
                    )
                )
                |> BackendTask.andThen
                    (\{ next, data } ->
                        let
                            nextAcc : List (List a)
                            nextAcc =
                                data :: acc
                        in
                        case next of
                            Just nextCursor ->
                                if False then
                                    go nextCursor nextAcc

                                else
                                    List.concat nextAcc
                                        |> BackendTask.succeed

                            Nothing ->
                                nextAcc
                                    |> List.reverse
                                    |> List.concat
                                    |> List.reverse
                                    |> BackendTask.succeed
                    )
    in
    go "" []


postsUrl : String -> String
postsUrl cursor =
    Url.Builder.crossOrigin
        "https://www.patreon.com"
        [ "api", "posts" ]
        [ Url.Builder.string "include"
            ([ "campaign"
             , "access_rules"
             , "access_rules.tier.null"
             , "attachments_media"
             , "audio"
             , "audio_preview.null"
             , "drop"
             , "images"
             , "media"
             , "native_video_insights"
             , "poll.choices"
             , "poll.current_user_responses.user"
             , "poll.current_user_responses.choice"
             , "poll.current_user_responses.poll"
             , "user"
             , "user_defined_tags"
             , "ti_checks"
             , "video.null"
             , "content_unlock_options.product_variant.null"
             ]
                |> String.join ","
            )
        , Url.Builder.string "fields[campaign]"
            ([ "currency"
             , "show_audio_post_download_links"
             , "avatar_photo_url"
             , "avatar_photo_image_urls"
             , "earnings_visibility"
             , "is_nsfw"
             , "is_monthly"
             , "name"
             , "url"
             ]
                |> String.join ","
            )
        , Url.Builder.string "fields[post]"
            ([ "change_visibility_at"
             , "comment_count"
             , "commenter_count"
             , "content"
             , "created_at"
             , "current_user_can_comment"
             , "current_user_can_delete"
             , "current_user_can_report"
             , "current_user_can_view"
             , "current_user_comment_disallowed_reason"
             , "current_user_has_liked"
             , "embed"
             , "image"
             , "insights_last_updated_at"
             , "is_paid"
             , "like_count"
             , "meta_image_url"
             , "min_cents_pledged_to_view"
             , "monetization_ineligibility_reason"
             , "post_file"
             , "post_metadata"
             , "published_at"
             , "patreon_url"
             , "post_type"
             , "pledge_url"
             , "preview_asset_type"
             , "thumbnail"
             , "thumbnail_url"
             , "teaser_text"
             , "title"
             , "upgrade_url"
             , "url"
             , "was_posted_by_campaign_owner"
             , "has_ti_violation"
             , "moderation_status"
             , "post_level_suspension_removal_date"
             , "pls_one_liners_by_category"
             , "video"
             , "video_preview"
             , "view_count"
             , "content_unlock_options"
             , "is_new_to_current_user"
             , "watch_state"
             ]
                |> String.join ","
            )
        , Url.Builder.string "fields[post_tag]"
            ([ "tag_type"
             , "value"
             ]
                |> String.join ","
            )
        , Url.Builder.string "fields[user]"
            ([ "image_url"
             , "full_name"
             , "url"
             ]
                |> String.join ","
            )
        , Url.Builder.string "fields[access_rule]"
            ([ "access_rule_type"
             , "amount_cents"
             ]
                |> String.join ","
            )
        , Url.Builder.string "fields[media]"
            ([ "id"
             , "image_urls"
             , "display"
             , "download_url"
             , "metadata"
             , "file_name"
             ]
                |> String.join ","
            )
        , Url.Builder.string "fields[native_video_insights]"
            ([ "average_view_duration"
             , "average_view_pct"
             , "has_preview"
             , "id"
             , "last_updated_at"
             , "num_views"
             , "preview_views"
             , "video_duration"
             ]
                |> String.join ","
            )
        , Url.Builder.string "fields[product-variant]"
            ([ "price_cents"
             , "currency_code"
             , "checkout_url"
             , "is_hidden"
             , "published_at_datetime"
             , "content_type"
             , "orders_count"
             , "access_metadata"
             ]
                |> String.join ","
            )
        , Url.Builder.string "fields[content-unlock-option]" "content_unlock_type"
        , Url.Builder.string "filter[campaign_id]" "119662"
        , Url.Builder.string "filter[contains_exclusive_posts]" "true"
        , Url.Builder.string "filter[is_draft]" "false"
        , Url.Builder.string "filter[accessible_by_user_id]" "26157566"
        , Url.Builder.string "sort" "-published_at"
        , Url.Builder.string "page[cursor]" cursor
        , Url.Builder.string "json-api-use-default-includes" "false"
        , Url.Builder.string "json-api-version" "1.0"
        ]


type Post
    = Post0 PostObject
    | Post1 PostMember
    | Post2 PostEntity
    | Post3 PostThing
    | Post4 PostInstance
    | Post5 PostConstituent
    | Post6 PostSpecimen
    | Post7 PostGadget
    | Post8 PostWidget
    | Post9 PostGizmo
    | Post10 PostPart
    | Post11 PostChunk
    | Post12 PostPiece
    | Post13 PostThingy
    | Post14 PostThingamajig
    | Post15 PostWhatsit
    | Post16 PostDoodad


type alias PostObject =
    { attributes : PostObjectAttributes
    , id : String
    , relationships : PostObjectRelationships
    , type_ : String
    }


type alias PostObjectAttributes =
    { commentCount : Int
    , commenterCount : Int
    , content : String
    , createdAt : String
    , currentUserCanComment : Bool
    , currentUserCanDelete : Bool
    , currentUserCanReport : Bool
    , currentUserCanView : Bool
    , currentUserHasLiked : Bool
    , hasTiViolation : Bool
    , image : Image
    , isNewToCurrentUser : Bool
    , isPaid : Bool
    , likeCount : Int
    , metaImageUrl : String
    , moderationStatus : String
    , patreonUrl : String
    , pledgeUrl : String
    , postFile : PostObjectAttributesPostFile
    , postMetadata : PostObjectAttributesPostMetadata
    , postType : String
    , previewAssetType : String
    , publishedAt : String
    , teaserText : String
    , thumbnail : PostObjectAttributesThumbnail
    , title : String
    , upgradeUrl : String
    , url : String
    , wasPostedByCampaignOwner : Bool
    }


type alias Image =
    { height : Int
    , largeUrl : String
    , thumbSquareLargeUrl : String
    , thumbSquareUrl : String
    , thumbUrl : String
    , url : String
    , width : Int
    }


type alias PostObjectAttributesPostFile =
    PostFile


type alias PostFile =
    { defaultThumbnail : HasUrl
    , duration : Int
    , fullContentDuration : Int
    , mediaId : Int
    , progress : Progress
    , state : String
    , url : String
    }


type alias HasUrl =
    { url : String
    }


type alias Progress =
    { isWatched : Bool
    , watchState : String
    }


type alias PostObjectAttributesPostMetadata =
    { imageOrder : List String
    , platform : PostObjectAttributesPostMetadataPlatform
    }


type alias PostObjectAttributesPostMetadataPlatform =
    {}


type alias PostObjectAttributesThumbnail =
    { large : String
    , large2 : String
    , square : String
    , url : String
    }


type alias PostObjectRelationships =
    { accessRules : PostObjectRelationshipsAccessRules
    , audio : PostObjectRelationshipsAudio
    , campaign : PostObjectRelationshipsCampaign
    , images : PostObjectRelationshipsImages
    , media : PostObjectRelationshipsMedia
    , user : PostObjectRelationshipsUser
    }


type alias PostObjectRelationshipsAccessRules =
    { data : List PostObjectRelationshipsAccessRulesDataObject
    }


type alias PostObjectRelationshipsAccessRulesDataObject =
    IdAndType


type alias IdAndType =
    { id : String
    , type_ : String
    }


type alias PostObjectRelationshipsAudio =
    { data : PostObjectRelationshipsAudioData
    , links : PostObjectRelationshipsAudioLinks
    }


type alias PostObjectRelationshipsAudioData =
    IdAndType


type alias PostObjectRelationshipsAudioLinks =
    { related : String
    }


type alias PostObjectRelationshipsCampaign =
    { data : PostObjectRelationshipsCampaignData
    , links : PostObjectRelationshipsCampaignLinks
    }


type alias PostObjectRelationshipsCampaignData =
    IdAndType


type alias PostObjectRelationshipsCampaignLinks =
    { related : String
    }


type alias PostObjectRelationshipsImages =
    { data : List PostObjectRelationshipsImagesDataObject
    }


type alias PostObjectRelationshipsImagesDataObject =
    IdAndType


type alias PostObjectRelationshipsMedia =
    { data : List PostObjectRelationshipsMediaDataObject
    }


type alias PostObjectRelationshipsMediaDataObject =
    IdAndType


type alias PostObjectRelationshipsUser =
    { data : PostObjectRelationshipsUserData
    , links : PostObjectRelationshipsUserLinks
    }


type alias PostObjectRelationshipsUserData =
    IdAndType


type alias PostObjectRelationshipsUserLinks =
    { related : String
    }


type alias PostMember =
    { attributes : PostMemberAttributes
    , id : String
    , relationships : PostMemberRelationships
    , type_ : String
    }


type alias PostMemberAttributes =
    { commentCount : Int
    , commenterCount : Int
    , content : String
    , createdAt : String
    , currentUserCanComment : Bool
    , currentUserCanDelete : Bool
    , currentUserCanReport : Bool
    , currentUserCanView : Bool
    , currentUserHasLiked : Bool
    , embed : PostMemberAttributesEmbed
    , hasTiViolation : Bool
    , image : Image
    , isNewToCurrentUser : Bool
    , isPaid : Bool
    , likeCount : Int
    , metaImageUrl : String
    , moderationStatus : String
    , patreonUrl : String
    , pledgeUrl : String
    , postFile : PostMemberAttributesPostFile
    , postMetadata : PostMemberAttributesPostMetadata
    , postType : String
    , previewAssetType : String
    , publishedAt : String
    , teaserText : String
    , thumbnail : PostMemberAttributesThumbnail
    , title : String
    , upgradeUrl : String
    , url : String
    , wasPostedByCampaignOwner : Bool
    }


type alias PostMemberAttributesEmbed =
    { description : String
    , html : String
    , provider : String
    , providerUrl : String
    , subject : String
    , url : String
    }


type alias PostMemberAttributesPostFile =
    { height : Int
    , imageColors : ImageColors
    , mediaId : Int
    , state : String
    , url : String
    , width : Int
    }


type alias ImageColors =
    { averageColorsOfCorners : AverageColorsOfCorners
    , dominantColor : String
    , palette : List String
    , textColor : String
    }


type alias AverageColorsOfCorners =
    { bottomLeft : String
    , bottomRight : String
    , topLeft : String
    , topRight : String
    }


type alias PostMemberAttributesPostMetadata =
    { platform : PostMemberAttributesPostMetadataPlatform
    }


type alias PostMemberAttributesPostMetadataPlatform =
    {}


type alias PostMemberAttributesThumbnail =
    { large : String
    , large2 : String
    , square : String
    , url : String
    }


type alias PostMemberRelationships =
    { accessRules : PostMemberRelationshipsAccessRules
    , campaign : PostMemberRelationshipsCampaign
    , images : PostMemberRelationshipsImages
    , media : PostMemberRelationshipsMedia
    , user : PostMemberRelationshipsUser
    }


type alias PostMemberRelationshipsAccessRules =
    { data : List PostMemberRelationshipsAccessRulesDataObject
    }


type alias PostMemberRelationshipsAccessRulesDataObject =
    IdAndType


type alias PostMemberRelationshipsCampaign =
    { data : PostMemberRelationshipsCampaignData
    , links : PostMemberRelationshipsCampaignLinks
    }


type alias PostMemberRelationshipsCampaignData =
    IdAndType


type alias PostMemberRelationshipsCampaignLinks =
    { related : String
    }


type alias PostMemberRelationshipsImages =
    { data : List PostMemberRelationshipsImagesDataObject
    }


type alias PostMemberRelationshipsImagesDataObject =
    IdAndType


type alias PostMemberRelationshipsMedia =
    { data : List PostMemberRelationshipsMediaDataObject
    }


type alias PostMemberRelationshipsMediaDataObject =
    IdAndType


type alias PostMemberRelationshipsUser =
    { data : PostMemberRelationshipsUserData
    , links : PostMemberRelationshipsUserLinks
    }


type alias PostMemberRelationshipsUserData =
    IdAndType


type alias PostMemberRelationshipsUserLinks =
    { related : String
    }


type alias PostEntity =
    { attributes : PostEntityAttributes
    , id : String
    , relationships : PostEntityRelationships
    , type_ : String
    }


type alias PostEntityAttributes =
    { commentCount : Int
    , commenterCount : Int
    , content : String
    , createdAt : String
    , currentUserCanComment : Bool
    , currentUserCanDelete : Bool
    , currentUserCanReport : Bool
    , currentUserCanView : Bool
    , currentUserHasLiked : Bool
    , hasTiViolation : Bool
    , isNewToCurrentUser : Bool
    , isPaid : Bool
    , likeCount : Int
    , metaImageUrl : String
    , moderationStatus : String
    , patreonUrl : String
    , pledgeUrl : String
    , postType : String
    , publishedAt : String
    , title : String
    , upgradeUrl : String
    , url : String
    , wasPostedByCampaignOwner : Bool
    }


type alias PostEntityRelationships =
    { accessRules : PostEntityRelationshipsAccessRules
    , campaign : PostEntityRelationshipsCampaign
    , user : PostEntityRelationshipsUser
    }


type alias PostEntityRelationshipsAccessRules =
    { data : List PostEntityRelationshipsAccessRulesDataObject
    }


type alias PostEntityRelationshipsAccessRulesDataObject =
    IdAndType


type alias PostEntityRelationshipsCampaign =
    { data : PostEntityRelationshipsCampaignData
    , links : PostEntityRelationshipsCampaignLinks
    }


type alias PostEntityRelationshipsCampaignData =
    IdAndType


type alias PostEntityRelationshipsCampaignLinks =
    { related : String
    }


type alias PostEntityRelationshipsUser =
    { data : PostEntityRelationshipsUserData
    , links : PostEntityRelationshipsUserLinks
    }


type alias PostEntityRelationshipsUserData =
    IdAndType


type alias PostEntityRelationshipsUserLinks =
    { related : String
    }


type alias PostThing =
    { attributes : PostThingAttributes
    , id : String
    , relationships : PostThingRelationships
    , type_ : String
    }


type alias PostThingAttributes =
    { commentCount : Int
    , commenterCount : Int
    , content : String
    , createdAt : String
    , currentUserCanComment : Bool
    , currentUserCanDelete : Bool
    , currentUserCanReport : Bool
    , currentUserCanView : Bool
    , currentUserHasLiked : Bool
    , hasTiViolation : Bool
    , image : Image
    , isNewToCurrentUser : Bool
    , isPaid : Bool
    , likeCount : Int
    , metaImageUrl : String
    , moderationStatus : String
    , patreonUrl : String
    , pledgeUrl : String
    , postFile : PostThingAttributesPostFile
    , postMetadata : PostThingAttributesPostMetadata
    , postType : String
    , previewAssetType : String
    , publishedAt : String
    , teaserText : String
    , thumbnail : PostThingAttributesThumbnail
    , title : String
    , upgradeUrl : String
    , url : String
    , wasPostedByCampaignOwner : Bool
    }


type alias PostThingAttributesPostFile =
    PostFile


type alias PostThingAttributesPostMetadata =
    { imageOrder : List String
    , platform : PostThingAttributesPostMetadataPlatform
    }


type alias PostThingAttributesPostMetadataPlatform =
    {}


type alias PostThingAttributesThumbnail =
    { large : String
    , large2 : String
    , square : String
    , url : String
    }


type alias PostThingRelationships =
    { accessRules : PostThingRelationshipsAccessRules
    , attachmentsMedia : PostThingRelationshipsAttachmentsMedia
    , audio : PostThingRelationshipsAudio
    , campaign : PostThingRelationshipsCampaign
    , images : PostThingRelationshipsImages
    , media : PostThingRelationshipsMedia
    , user : PostThingRelationshipsUser
    }


type alias PostThingRelationshipsAccessRules =
    { data : List PostThingRelationshipsAccessRulesDataObject
    }


type alias PostThingRelationshipsAccessRulesDataObject =
    IdAndType


type alias PostThingRelationshipsAttachmentsMedia =
    { data : List PostThingRelationshipsAttachmentsMediaDataObject
    }


type alias PostThingRelationshipsAttachmentsMediaDataObject =
    IdAndType


type alias PostThingRelationshipsAudio =
    { data : PostThingRelationshipsAudioData
    , links : PostThingRelationshipsAudioLinks
    }


type alias PostThingRelationshipsAudioData =
    IdAndType


type alias PostThingRelationshipsAudioLinks =
    { related : String
    }


type alias PostThingRelationshipsCampaign =
    { data : PostThingRelationshipsCampaignData
    , links : PostThingRelationshipsCampaignLinks
    }


type alias PostThingRelationshipsCampaignData =
    IdAndType


type alias PostThingRelationshipsCampaignLinks =
    { related : String
    }


type alias PostThingRelationshipsImages =
    { data : List PostThingRelationshipsImagesDataObject
    }


type alias PostThingRelationshipsImagesDataObject =
    IdAndType


type alias PostThingRelationshipsMedia =
    { data : List PostThingRelationshipsMediaDataObject
    }


type alias PostThingRelationshipsMediaDataObject =
    IdAndType


type alias PostThingRelationshipsUser =
    { data : PostThingRelationshipsUserData
    , links : PostThingRelationshipsUserLinks
    }


type alias PostThingRelationshipsUserData =
    IdAndType


type alias PostThingRelationshipsUserLinks =
    { related : String
    }


type alias PostInstance =
    { attributes : PostInstanceAttributes
    , id : String
    , relationships : PostInstanceRelationships
    , type_ : String
    }


type alias PostInstanceAttributes =
    { commentCount : Int
    , commenterCount : Int
    , content : String
    , createdAt : String
    , currentUserCanComment : Bool
    , currentUserCanDelete : Bool
    , currentUserCanReport : Bool
    , currentUserCanView : Bool
    , currentUserHasLiked : Bool
    , hasTiViolation : Bool
    , image : Image
    , isNewToCurrentUser : Bool
    , isPaid : Bool
    , likeCount : Int
    , metaImageUrl : String
    , moderationStatus : String
    , patreonUrl : String
    , pledgeUrl : String
    , postFile : PostInstanceAttributesPostFile
    , postMetadata : PostInstanceAttributesPostMetadata
    , postType : String
    , previewAssetType : String
    , publishedAt : String
    , teaserText : String
    , thumbnail : PostInstanceAttributesThumbnail
    , title : String
    , upgradeUrl : String
    , url : String
    , wasPostedByCampaignOwner : Bool
    }


type alias PostInstanceAttributesPostFile =
    { height : Int
    , imageColors : PostInstanceAttributesPostFileImageColors
    , mediaId : Int
    , state : String
    , url : String
    , width : Int
    }


type alias PostInstanceAttributesPostFileImageColors =
    ImageColors


type alias PostInstanceAttributesPostMetadata =
    { imageOrder : List String
    , platform : PostInstanceAttributesPostMetadataPlatform
    }


type alias PostInstanceAttributesPostMetadataPlatform =
    {}


type alias PostInstanceAttributesThumbnail =
    { large : String
    , large2 : String
    , square : String
    , url : String
    }


type alias PostInstanceRelationships =
    { accessRules : PostInstanceRelationshipsAccessRules
    , campaign : PostInstanceRelationshipsCampaign
    , images : PostInstanceRelationshipsImages
    , media : PostInstanceRelationshipsMedia
    , user : PostInstanceRelationshipsUser
    }


type alias PostInstanceRelationshipsAccessRules =
    { data : List PostInstanceRelationshipsAccessRulesDataObject
    }


type alias PostInstanceRelationshipsAccessRulesDataObject =
    IdAndType


type alias PostInstanceRelationshipsCampaign =
    { data : PostInstanceRelationshipsCampaignData
    , links : PostInstanceRelationshipsCampaignLinks
    }


type alias PostInstanceRelationshipsCampaignData =
    IdAndType


type alias PostInstanceRelationshipsCampaignLinks =
    { related : String
    }


type alias PostInstanceRelationshipsImages =
    { data : List PostInstanceRelationshipsImagesDataObject
    }


type alias PostInstanceRelationshipsImagesDataObject =
    IdAndType


type alias PostInstanceRelationshipsMedia =
    { data : List PostInstanceRelationshipsMediaDataObject
    }


type alias PostInstanceRelationshipsMediaDataObject =
    IdAndType


type alias PostInstanceRelationshipsUser =
    { data : PostInstanceRelationshipsUserData
    , links : PostInstanceRelationshipsUserLinks
    }


type alias PostInstanceRelationshipsUserData =
    IdAndType


type alias PostInstanceRelationshipsUserLinks =
    { related : String
    }


type alias PostConstituent =
    { attributes : PostConstituentAttributes
    , id : String
    , relationships : PostConstituentRelationships
    , type_ : String
    }


type alias PostConstituentAttributes =
    { commentCount : Int
    , commenterCount : Int
    , content : String
    , createdAt : String
    , currentUserCanComment : Bool
    , currentUserCanDelete : Bool
    , currentUserCanReport : Bool
    , currentUserCanView : Bool
    , currentUserHasLiked : Bool
    , hasTiViolation : Bool
    , image : Image
    , isNewToCurrentUser : Bool
    , isPaid : Bool
    , likeCount : Int
    , metaImageUrl : String
    , moderationStatus : String
    , patreonUrl : String
    , pledgeUrl : String
    , postFile : PostConstituentAttributesPostFile
    , postMetadata : PostConstituentAttributesPostMetadata
    , postType : String
    , publishedAt : String
    , thumbnail : PostConstituentAttributesThumbnail
    , title : String
    , upgradeUrl : String
    , url : String
    , wasPostedByCampaignOwner : Bool
    }


type alias PostConstituentAttributesPostFile =
    { height : Int
    , imageColors : PostConstituentAttributesPostFileImageColors
    , mediaId : Int
    , state : String
    , url : String
    , width : Int
    }


type alias PostConstituentAttributesPostFileImageColors =
    ImageColors


type alias PostConstituentAttributesPostMetadata =
    { imageOrder : List String
    }


type alias PostConstituentAttributesThumbnail =
    { large : String
    , large2 : String
    , square : String
    , url : String
    }


type alias PostConstituentRelationships =
    { accessRules : PostConstituentRelationshipsAccessRules
    , campaign : PostConstituentRelationshipsCampaign
    , images : PostConstituentRelationshipsImages
    , media : PostConstituentRelationshipsMedia
    , user : PostConstituentRelationshipsUser
    }


type alias PostConstituentRelationshipsAccessRules =
    { data : List PostConstituentRelationshipsAccessRulesDataObject
    }


type alias PostConstituentRelationshipsAccessRulesDataObject =
    IdAndType


type alias PostConstituentRelationshipsCampaign =
    { data : PostConstituentRelationshipsCampaignData
    , links : PostConstituentRelationshipsCampaignLinks
    }


type alias PostConstituentRelationshipsCampaignData =
    IdAndType


type alias PostConstituentRelationshipsCampaignLinks =
    { related : String
    }


type alias PostConstituentRelationshipsImages =
    { data : List PostConstituentRelationshipsImagesDataObject
    }


type alias PostConstituentRelationshipsImagesDataObject =
    IdAndType


type alias PostConstituentRelationshipsMedia =
    { data : List PostConstituentRelationshipsMediaDataObject
    }


type alias PostConstituentRelationshipsMediaDataObject =
    IdAndType


type alias PostConstituentRelationshipsUser =
    { data : PostConstituentRelationshipsUserData
    , links : PostConstituentRelationshipsUserLinks
    }


type alias PostConstituentRelationshipsUserData =
    IdAndType


type alias PostConstituentRelationshipsUserLinks =
    { related : String
    }


type alias PostSpecimen =
    { attributes : PostSpecimenAttributes
    , id : String
    , relationships : PostSpecimenRelationships
    , type_ : String
    }


type alias PostSpecimenAttributes =
    { commentCount : Int
    , commenterCount : Int
    , content : String
    , createdAt : String
    , currentUserCanComment : Bool
    , currentUserCanDelete : Bool
    , currentUserCanReport : Bool
    , currentUserCanView : Bool
    , currentUserHasLiked : Bool
    , embed : PostSpecimenAttributesEmbed
    , hasTiViolation : Bool
    , image : Image
    , isNewToCurrentUser : Bool
    , isPaid : Bool
    , likeCount : Int
    , metaImageUrl : String
    , moderationStatus : String
    , patreonUrl : String
    , pledgeUrl : String
    , postFile : PostSpecimenAttributesPostFile
    , postMetadata : PostSpecimenAttributesPostMetadata
    , postType : String
    , previewAssetType : String
    , publishedAt : String
    , teaserText : String
    , thumbnail : PostSpecimenAttributesThumbnail
    , title : String
    , upgradeUrl : String
    , url : String
    , wasPostedByCampaignOwner : Bool
    }


type alias PostSpecimenAttributesEmbed =
    { description : String
    , provider : String
    , providerUrl : String
    , subject : String
    , url : String
    }


type alias PostSpecimenAttributesPostFile =
    { height : Int
    , imageColors : PostSpecimenAttributesPostFileImageColors
    , mediaId : Int
    , state : String
    , url : String
    , width : Int
    }


type alias PostSpecimenAttributesPostFileImageColors =
    ImageColors


type alias PostSpecimenAttributesPostMetadata =
    { platform : PostSpecimenAttributesPostMetadataPlatform
    }


type alias PostSpecimenAttributesPostMetadataPlatform =
    {}


type alias PostSpecimenAttributesThumbnail =
    { large : String
    , large2 : String
    , square : String
    , url : String
    }


type alias PostSpecimenRelationships =
    { accessRules : PostSpecimenRelationshipsAccessRules
    , campaign : PostSpecimenRelationshipsCampaign
    , images : PostSpecimenRelationshipsImages
    , media : PostSpecimenRelationshipsMedia
    , user : PostSpecimenRelationshipsUser
    , userDefinedTags : PostSpecimenRelationshipsUserDefinedTags
    }


type alias PostSpecimenRelationshipsAccessRules =
    { data : List PostSpecimenRelationshipsAccessRulesDataObject
    }


type alias PostSpecimenRelationshipsAccessRulesDataObject =
    IdAndType


type alias PostSpecimenRelationshipsCampaign =
    { data : PostSpecimenRelationshipsCampaignData
    , links : PostSpecimenRelationshipsCampaignLinks
    }


type alias PostSpecimenRelationshipsCampaignData =
    IdAndType


type alias PostSpecimenRelationshipsCampaignLinks =
    { related : String
    }


type alias PostSpecimenRelationshipsImages =
    { data : List PostSpecimenRelationshipsImagesDataObject
    }


type alias PostSpecimenRelationshipsImagesDataObject =
    IdAndType


type alias PostSpecimenRelationshipsMedia =
    { data : List PostSpecimenRelationshipsMediaDataObject
    }


type alias PostSpecimenRelationshipsMediaDataObject =
    IdAndType


type alias PostSpecimenRelationshipsUser =
    { data : PostSpecimenRelationshipsUserData
    , links : PostSpecimenRelationshipsUserLinks
    }


type alias PostSpecimenRelationshipsUserData =
    IdAndType


type alias PostSpecimenRelationshipsUserLinks =
    { related : String
    }


type alias PostSpecimenRelationshipsUserDefinedTags =
    { data : List PostSpecimenRelationshipsUserDefinedTagsDataObject
    }


type alias PostSpecimenRelationshipsUserDefinedTagsDataObject =
    IdAndType


type alias PostGadget =
    { attributes : PostGadgetAttributes
    , id : String
    , relationships : PostGadgetRelationships
    , type_ : String
    }


type alias PostGadgetAttributes =
    { commentCount : Int
    , commenterCount : Int
    , content : String
    , createdAt : String
    , currentUserCanComment : Bool
    , currentUserCanDelete : Bool
    , currentUserCanReport : Bool
    , currentUserCanView : Bool
    , currentUserHasLiked : Bool
    , hasTiViolation : Bool
    , image : Image
    , isNewToCurrentUser : Bool
    , isPaid : Bool
    , likeCount : Int
    , metaImageUrl : String
    , moderationStatus : String
    , patreonUrl : String
    , pledgeUrl : String
    , postFile : PostGadgetAttributesPostFile
    , postMetadata : PostGadgetAttributesPostMetadata
    , postType : String
    , previewAssetType : String
    , publishedAt : String
    , teaserText : String
    , thumbnail : PostGadgetAttributesThumbnail
    , title : String
    , upgradeUrl : String
    , url : String
    , wasPostedByCampaignOwner : Bool
    }


type alias PostGadgetAttributesPostFile =
    PostFile


type alias PostGadgetAttributesPostMetadata =
    { episodeNumber : Int
    , platform : PostGadgetAttributesPostMetadataPlatform
    , season : Int
    }


type alias PostGadgetAttributesPostMetadataPlatform =
    {}


type alias PostGadgetAttributesThumbnail =
    { large : String
    , large2 : String
    , square : String
    , url : String
    }


type alias PostGadgetRelationships =
    { accessRules : PostGadgetRelationshipsAccessRules
    , audio : PostGadgetRelationshipsAudio
    , campaign : PostGadgetRelationshipsCampaign
    , media : PostGadgetRelationshipsMedia
    , user : PostGadgetRelationshipsUser
    , userDefinedTags : PostGadgetRelationshipsUserDefinedTags
    }


type alias PostGadgetRelationshipsAccessRules =
    { data : List PostGadgetRelationshipsAccessRulesDataObject
    }


type alias PostGadgetRelationshipsAccessRulesDataObject =
    IdAndType


type alias PostGadgetRelationshipsAudio =
    { data : PostGadgetRelationshipsAudioData
    , links : PostGadgetRelationshipsAudioLinks
    }


type alias PostGadgetRelationshipsAudioData =
    IdAndType


type alias PostGadgetRelationshipsAudioLinks =
    { related : String
    }


type alias PostGadgetRelationshipsCampaign =
    { data : PostGadgetRelationshipsCampaignData
    , links : PostGadgetRelationshipsCampaignLinks
    }


type alias PostGadgetRelationshipsCampaignData =
    IdAndType


type alias PostGadgetRelationshipsCampaignLinks =
    { related : String
    }


type alias PostGadgetRelationshipsMedia =
    { data : List PostGadgetRelationshipsMediaDataObject
    }


type alias PostGadgetRelationshipsMediaDataObject =
    IdAndType


type alias PostGadgetRelationshipsUser =
    { data : PostGadgetRelationshipsUserData
    , links : PostGadgetRelationshipsUserLinks
    }


type alias PostGadgetRelationshipsUserData =
    IdAndType


type alias PostGadgetRelationshipsUserLinks =
    { related : String
    }


type alias PostGadgetRelationshipsUserDefinedTags =
    { data : List PostGadgetRelationshipsUserDefinedTagsDataObject
    }


type alias PostGadgetRelationshipsUserDefinedTagsDataObject =
    IdAndType


type alias PostWidget =
    { attributes : PostWidgetAttributes
    , id : String
    , relationships : PostWidgetRelationships
    , type_ : String
    }


type alias PostWidgetAttributes =
    { commentCount : Int
    , commenterCount : Int
    , content : String
    , createdAt : String
    , currentUserCanComment : Bool
    , currentUserCanDelete : Bool
    , currentUserCanReport : Bool
    , currentUserCanView : Bool
    , currentUserHasLiked : Bool
    , embed : PostWidgetAttributesEmbed
    , hasTiViolation : Bool
    , image : Image
    , isNewToCurrentUser : Bool
    , isPaid : Bool
    , likeCount : Int
    , metaImageUrl : String
    , moderationStatus : String
    , patreonUrl : String
    , pledgeUrl : String
    , postFile : PostWidgetAttributesPostFile
    , postMetadata : PostWidgetAttributesPostMetadata
    , postType : String
    , previewAssetType : String
    , publishedAt : String
    , teaserText : String
    , thumbnail : PostWidgetAttributesThumbnail
    , title : String
    , upgradeUrl : String
    , url : String
    , wasPostedByCampaignOwner : Bool
    }


type alias PostWidgetAttributesEmbed =
    { description : String
    , html : String
    , provider : String
    , providerUrl : String
    , subject : String
    , url : String
    }


type alias PostWidgetAttributesPostFile =
    { height : Int
    , imageColors : PostWidgetAttributesPostFileImageColors
    , mediaId : Int
    , state : String
    , url : String
    , width : Int
    }


type alias PostWidgetAttributesPostFileImageColors =
    ImageColors


type alias PostWidgetAttributesPostMetadata =
    { platform : PostWidgetAttributesPostMetadataPlatform
    }


type alias PostWidgetAttributesPostMetadataPlatform =
    {}


type alias PostWidgetAttributesThumbnail =
    { large : String
    , large2 : String
    , square : String
    , url : String
    }


type alias PostWidgetRelationships =
    { accessRules : PostWidgetRelationshipsAccessRules
    , campaign : PostWidgetRelationshipsCampaign
    , images : PostWidgetRelationshipsImages
    , media : PostWidgetRelationshipsMedia
    , user : PostWidgetRelationshipsUser
    , userDefinedTags : PostWidgetRelationshipsUserDefinedTags
    }


type alias PostWidgetRelationshipsAccessRules =
    { data : List PostWidgetRelationshipsAccessRulesDataObject
    }


type alias PostWidgetRelationshipsAccessRulesDataObject =
    IdAndType


type alias PostWidgetRelationshipsCampaign =
    { data : PostWidgetRelationshipsCampaignData
    , links : PostWidgetRelationshipsCampaignLinks
    }


type alias PostWidgetRelationshipsCampaignData =
    IdAndType


type alias PostWidgetRelationshipsCampaignLinks =
    { related : String
    }


type alias PostWidgetRelationshipsImages =
    { data : List PostWidgetRelationshipsImagesDataObject
    }


type alias PostWidgetRelationshipsImagesDataObject =
    IdAndType


type alias PostWidgetRelationshipsMedia =
    { data : List PostWidgetRelationshipsMediaDataObject
    }


type alias PostWidgetRelationshipsMediaDataObject =
    IdAndType


type alias PostWidgetRelationshipsUser =
    { data : PostWidgetRelationshipsUserData
    , links : PostWidgetRelationshipsUserLinks
    }


type alias PostWidgetRelationshipsUserData =
    IdAndType


type alias PostWidgetRelationshipsUserLinks =
    { related : String
    }


type alias PostWidgetRelationshipsUserDefinedTags =
    { data : List PostWidgetRelationshipsUserDefinedTagsDataObject
    }


type alias PostWidgetRelationshipsUserDefinedTagsDataObject =
    IdAndType


type alias PostGizmo =
    { attributes : PostGizmoAttributes
    , id : String
    , relationships : PostGizmoRelationships
    , type_ : String
    }


type alias PostGizmoAttributes =
    { commentCount : Int
    , commenterCount : Int
    , content : String
    , createdAt : String
    , currentUserCanComment : Bool
    , currentUserCanDelete : Bool
    , currentUserCanReport : Bool
    , currentUserCanView : Bool
    , currentUserHasLiked : Bool
    , hasTiViolation : Bool
    , image : Image
    , isNewToCurrentUser : Bool
    , isPaid : Bool
    , likeCount : Int
    , metaImageUrl : String
    , moderationStatus : String
    , patreonUrl : String
    , pledgeUrl : String
    , postFile : PostGizmoAttributesPostFile
    , postMetadata : PostGizmoAttributesPostMetadata
    , postType : String
    , previewAssetType : String
    , publishedAt : String
    , teaserText : String
    , thumbnail : PostGizmoAttributesThumbnail
    , title : String
    , upgradeUrl : String
    , url : String
    , wasPostedByCampaignOwner : Bool
    }


type alias PostGizmoAttributesPostFile =
    PostFile


type alias PostGizmoAttributesPostMetadata =
    { imageOrder : List String
    , platform : PostGizmoAttributesPostMetadataPlatform
    }


type alias PostGizmoAttributesPostMetadataPlatform =
    {}


type alias PostGizmoAttributesThumbnail =
    { large : String
    , large2 : String
    , square : String
    , url : String
    }


type alias PostGizmoRelationships =
    { accessRules : PostGizmoRelationshipsAccessRules
    , audio : PostGizmoRelationshipsAudio
    , audioPreview : PostGizmoRelationshipsAudioPreview
    , campaign : PostGizmoRelationshipsCampaign
    , images : PostGizmoRelationshipsImages
    , media : PostGizmoRelationshipsMedia
    , user : PostGizmoRelationshipsUser
    }


type alias PostGizmoRelationshipsAccessRules =
    { data : List PostGizmoRelationshipsAccessRulesDataObject
    }


type alias PostGizmoRelationshipsAccessRulesDataObject =
    IdAndType


type alias PostGizmoRelationshipsAudio =
    { data : PostGizmoRelationshipsAudioData
    , links : PostGizmoRelationshipsAudioLinks
    }


type alias PostGizmoRelationshipsAudioData =
    IdAndType


type alias PostGizmoRelationshipsAudioLinks =
    { related : String
    }


type alias PostGizmoRelationshipsAudioPreview =
    { data : PostGizmoRelationshipsAudioPreviewData
    , links : PostGizmoRelationshipsAudioPreviewLinks
    }


type alias PostGizmoRelationshipsAudioPreviewData =
    IdAndType


type alias PostGizmoRelationshipsAudioPreviewLinks =
    { related : String
    }


type alias PostGizmoRelationshipsCampaign =
    { data : PostGizmoRelationshipsCampaignData
    , links : PostGizmoRelationshipsCampaignLinks
    }


type alias PostGizmoRelationshipsCampaignData =
    IdAndType


type alias PostGizmoRelationshipsCampaignLinks =
    { related : String
    }


type alias PostGizmoRelationshipsImages =
    { data : List PostGizmoRelationshipsImagesDataObject
    }


type alias PostGizmoRelationshipsImagesDataObject =
    IdAndType


type alias PostGizmoRelationshipsMedia =
    { data : List PostGizmoRelationshipsMediaDataObject
    }


type alias PostGizmoRelationshipsMediaDataObject =
    IdAndType


type alias PostGizmoRelationshipsUser =
    { data : PostGizmoRelationshipsUserData
    , links : PostGizmoRelationshipsUserLinks
    }


type alias PostGizmoRelationshipsUserData =
    IdAndType


type alias PostGizmoRelationshipsUserLinks =
    { related : String
    }


type alias PostPart =
    { attributes : PostPartAttributes
    , id : String
    , relationships : PostPartRelationships
    , type_ : String
    }


type alias PostPartAttributes =
    { commentCount : Int
    , commenterCount : Int
    , content : String
    , createdAt : String
    , currentUserCanComment : Bool
    , currentUserCanDelete : Bool
    , currentUserCanReport : Bool
    , currentUserCanView : Bool
    , currentUserHasLiked : Bool
    , hasTiViolation : Bool
    , image : Image
    , isNewToCurrentUser : Bool
    , isPaid : Bool
    , likeCount : Int
    , metaImageUrl : String
    , moderationStatus : String
    , patreonUrl : String
    , pledgeUrl : String
    , postFile : PostPartAttributesPostFile
    , postMetadata : PostPartAttributesPostMetadata
    , postType : String
    , previewAssetType : String
    , publishedAt : String
    , teaserText : String
    , thumbnail : PostPartAttributesThumbnail
    , title : String
    , upgradeUrl : String
    , url : String
    , wasPostedByCampaignOwner : Bool
    }


type alias PostPartAttributesPostFile =
    PostFile


type alias PostPartAttributesPostMetadata =
    { platform : PostPartAttributesPostMetadataPlatform
    }


type alias PostPartAttributesPostMetadataPlatform =
    {}


type alias PostPartAttributesThumbnail =
    { large : String
    , large2 : String
    , square : String
    , url : String
    }


type alias PostPartRelationships =
    { accessRules : PostPartRelationshipsAccessRules
    , attachmentsMedia : PostPartRelationshipsAttachmentsMedia
    , audio : PostPartRelationshipsAudio
    , audioPreview : PostPartRelationshipsAudioPreview
    , campaign : PostPartRelationshipsCampaign
    , media : PostPartRelationshipsMedia
    , user : PostPartRelationshipsUser
    , userDefinedTags : PostPartRelationshipsUserDefinedTags
    }


type alias PostPartRelationshipsAccessRules =
    { data : List PostPartRelationshipsAccessRulesDataObject
    }


type alias PostPartRelationshipsAccessRulesDataObject =
    IdAndType


type alias PostPartRelationshipsAttachmentsMedia =
    { data : List PostPartRelationshipsAttachmentsMediaDataObject
    }


type alias PostPartRelationshipsAttachmentsMediaDataObject =
    IdAndType


type alias PostPartRelationshipsAudio =
    { data : PostPartRelationshipsAudioData
    , links : PostPartRelationshipsAudioLinks
    }


type alias PostPartRelationshipsAudioData =
    IdAndType


type alias PostPartRelationshipsAudioLinks =
    { related : String
    }


type alias PostPartRelationshipsAudioPreview =
    { data : PostPartRelationshipsAudioPreviewData
    , links : PostPartRelationshipsAudioPreviewLinks
    }


type alias PostPartRelationshipsAudioPreviewData =
    IdAndType


type alias PostPartRelationshipsAudioPreviewLinks =
    { related : String
    }


type alias PostPartRelationshipsCampaign =
    { data : PostPartRelationshipsCampaignData
    , links : PostPartRelationshipsCampaignLinks
    }


type alias PostPartRelationshipsCampaignData =
    IdAndType


type alias PostPartRelationshipsCampaignLinks =
    { related : String
    }


type alias PostPartRelationshipsMedia =
    { data : List PostPartRelationshipsMediaDataObject
    }


type alias PostPartRelationshipsMediaDataObject =
    IdAndType


type alias PostPartRelationshipsUser =
    { data : PostPartRelationshipsUserData
    , links : PostPartRelationshipsUserLinks
    }


type alias PostPartRelationshipsUserData =
    IdAndType


type alias PostPartRelationshipsUserLinks =
    { related : String
    }


type alias PostPartRelationshipsUserDefinedTags =
    { data : List PostPartRelationshipsUserDefinedTagsDataObject
    }


type alias PostPartRelationshipsUserDefinedTagsDataObject =
    IdAndType


type alias PostChunk =
    { attributes : PostChunkAttributes
    , id : String
    , relationships : PostChunkRelationships
    , type_ : String
    }


type alias PostChunkAttributes =
    { commentCount : Int
    , commenterCount : Int
    , content : String
    , createdAt : String
    , currentUserCanComment : Bool
    , currentUserCanDelete : Bool
    , currentUserCanReport : Bool
    , currentUserCanView : Bool
    , currentUserHasLiked : Bool
    , embed : PostChunkAttributesEmbed
    , hasTiViolation : Bool
    , image : Image
    , isNewToCurrentUser : Bool
    , isPaid : Bool
    , likeCount : Int
    , metaImageUrl : String
    , minCentsPledgedToView : Int
    , moderationStatus : String
    , patreonUrl : String
    , pledgeUrl : String
    , postFile : PostChunkAttributesPostFile
    , postMetadata : PostChunkAttributesPostMetadata
    , postType : String
    , previewAssetType : String
    , publishedAt : String
    , teaserText : String
    , thumbnail : PostChunkAttributesThumbnail
    , title : String
    , upgradeUrl : String
    , url : String
    , wasPostedByCampaignOwner : Bool
    }


type alias PostChunkAttributesEmbed =
    { description : String
    , html : String
    , provider : String
    , providerUrl : String
    , subject : String
    , url : String
    }


type alias PostChunkAttributesPostFile =
    { height : Int
    , imageColors : PostChunkAttributesPostFileImageColors
    , mediaId : Int
    , state : String
    , url : String
    , width : Int
    }


type alias PostChunkAttributesPostFileImageColors =
    ImageColors


type alias PostChunkAttributesPostMetadata =
    { platform : PostChunkAttributesPostMetadataPlatform
    }


type alias PostChunkAttributesPostMetadataPlatform =
    {}


type alias PostChunkAttributesThumbnail =
    { large : String
    , large2 : String
    , square : String
    , url : String
    }


type alias PostChunkRelationships =
    { accessRules : PostChunkRelationshipsAccessRules
    , campaign : PostChunkRelationshipsCampaign
    , images : PostChunkRelationshipsImages
    , media : PostChunkRelationshipsMedia
    , user : PostChunkRelationshipsUser
    }


type alias PostChunkRelationshipsAccessRules =
    { data : List PostChunkRelationshipsAccessRulesDataObject
    }


type alias PostChunkRelationshipsAccessRulesDataObject =
    IdAndType


type alias PostChunkRelationshipsCampaign =
    { data : PostChunkRelationshipsCampaignData
    , links : PostChunkRelationshipsCampaignLinks
    }


type alias PostChunkRelationshipsCampaignData =
    IdAndType


type alias PostChunkRelationshipsCampaignLinks =
    { related : String
    }


type alias PostChunkRelationshipsImages =
    { data : List PostChunkRelationshipsImagesDataObject
    }


type alias PostChunkRelationshipsImagesDataObject =
    IdAndType


type alias PostChunkRelationshipsMedia =
    { data : List PostChunkRelationshipsMediaDataObject
    }


type alias PostChunkRelationshipsMediaDataObject =
    IdAndType


type alias PostChunkRelationshipsUser =
    { data : PostChunkRelationshipsUserData
    , links : PostChunkRelationshipsUserLinks
    }


type alias PostChunkRelationshipsUserData =
    IdAndType


type alias PostChunkRelationshipsUserLinks =
    { related : String
    }


type alias PostPiece =
    { attributes : PostPieceAttributes
    , id : String
    , relationships : PostPieceRelationships
    , type_ : String
    }


type alias PostPieceAttributes =
    { commentCount : Int
    , commenterCount : Int
    , content : String
    , createdAt : String
    , currentUserCanComment : Bool
    , currentUserCanDelete : Bool
    , currentUserCanReport : Bool
    , currentUserCanView : Bool
    , currentUserHasLiked : Bool
    , embed : PostPieceAttributesEmbed
    , hasTiViolation : Bool
    , image : Image
    , isNewToCurrentUser : Bool
    , isPaid : Bool
    , likeCount : Int
    , metaImageUrl : String
    , moderationStatus : String
    , patreonUrl : String
    , pledgeUrl : String
    , postFile : PostPieceAttributesPostFile
    , postMetadata : PostPieceAttributesPostMetadata
    , postType : String
    , previewAssetType : String
    , publishedAt : String
    , teaserText : String
    , thumbnail : PostPieceAttributesThumbnail
    , title : String
    , upgradeUrl : String
    , url : String
    , wasPostedByCampaignOwner : Bool
    }


type alias PostPieceAttributesEmbed =
    { description : String
    , html : String
    , provider : String
    , providerUrl : String
    , subject : String
    , url : String
    }


type alias PostPieceAttributesPostFile =
    { height : Int
    , imageColors : PostPieceAttributesPostFileImageColors
    , mediaId : Int
    , state : String
    , url : String
    , width : Int
    }


type alias PostPieceAttributesPostFileImageColors =
    ImageColors


type alias PostPieceAttributesPostMetadata =
    { platform : PostPieceAttributesPostMetadataPlatform
    }


type alias PostPieceAttributesPostMetadataPlatform =
    {}


type alias PostPieceAttributesThumbnail =
    { large : String
    , large2 : String
    , square : String
    , url : String
    }


type alias PostPieceRelationships =
    { accessRules : PostPieceRelationshipsAccessRules
    , campaign : PostPieceRelationshipsCampaign
    , images : PostPieceRelationshipsImages
    , media : PostPieceRelationshipsMedia
    , user : PostPieceRelationshipsUser
    }


type alias PostPieceRelationshipsAccessRules =
    { data : List PostPieceRelationshipsAccessRulesDataObject
    }


type alias PostPieceRelationshipsAccessRulesDataObject =
    IdAndType


type alias PostPieceRelationshipsCampaign =
    { data : PostPieceRelationshipsCampaignData
    , links : PostPieceRelationshipsCampaignLinks
    }


type alias PostPieceRelationshipsCampaignData =
    IdAndType


type alias PostPieceRelationshipsCampaignLinks =
    { related : String
    }


type alias PostPieceRelationshipsImages =
    { data : List PostPieceRelationshipsImagesDataObject
    }


type alias PostPieceRelationshipsImagesDataObject =
    IdAndType


type alias PostPieceRelationshipsMedia =
    { data : List PostPieceRelationshipsMediaDataObject
    }


type alias PostPieceRelationshipsMediaDataObject =
    IdAndType


type alias PostPieceRelationshipsUser =
    { data : PostPieceRelationshipsUserData
    , links : PostPieceRelationshipsUserLinks
    }


type alias PostPieceRelationshipsUserData =
    IdAndType


type alias PostPieceRelationshipsUserLinks =
    { related : String
    }


type alias PostThingy =
    { attributes : PostThingyAttributes
    , id : String
    , relationships : PostThingyRelationships
    , type_ : String
    }


type alias PostThingyAttributes =
    { commentCount : Int
    , commenterCount : Int
    , content : String
    , createdAt : String
    , currentUserCanComment : Bool
    , currentUserCanDelete : Bool
    , currentUserCanReport : Bool
    , currentUserCanView : Bool
    , currentUserHasLiked : Bool
    , hasTiViolation : Bool
    , image : Image
    , isNewToCurrentUser : Bool
    , isPaid : Bool
    , likeCount : Int
    , metaImageUrl : String
    , moderationStatus : String
    , patreonUrl : String
    , pledgeUrl : String
    , postFile : PostThingyAttributesPostFile
    , postMetadata : PostThingyAttributesPostMetadata
    , postType : String
    , previewAssetType : String
    , publishedAt : String
    , teaserText : String
    , thumbnail : PostThingyAttributesThumbnail
    , title : String
    , upgradeUrl : String
    , url : String
    , wasPostedByCampaignOwner : Bool
    }


type alias PostThingyAttributesPostFile =
    { height : Int
    , imageColors : PostThingyAttributesPostFileImageColors
    , mediaId : Int
    , state : String
    , url : String
    , width : Int
    }


type alias PostThingyAttributesPostFileImageColors =
    ImageColors


type alias PostThingyAttributesPostMetadata =
    { imageOrder : List String
    , platform : PostThingyAttributesPostMetadataPlatform
    }


type alias PostThingyAttributesPostMetadataPlatform =
    {}


type alias PostThingyAttributesThumbnail =
    { large : String
    , large2 : String
    , square : String
    , url : String
    }


type alias PostThingyRelationships =
    { accessRules : PostThingyRelationshipsAccessRules
    , campaign : PostThingyRelationshipsCampaign
    , images : PostThingyRelationshipsImages
    , media : PostThingyRelationshipsMedia
    , user : PostThingyRelationshipsUser
    }


type alias PostThingyRelationshipsAccessRules =
    { data : List PostThingyRelationshipsAccessRulesDataObject
    }


type alias PostThingyRelationshipsAccessRulesDataObject =
    IdAndType


type alias PostThingyRelationshipsCampaign =
    { data : PostThingyRelationshipsCampaignData
    , links : PostThingyRelationshipsCampaignLinks
    }


type alias PostThingyRelationshipsCampaignData =
    IdAndType


type alias PostThingyRelationshipsCampaignLinks =
    { related : String
    }


type alias PostThingyRelationshipsImages =
    { data : List PostThingyRelationshipsImagesDataObject
    }


type alias PostThingyRelationshipsImagesDataObject =
    IdAndType


type alias PostThingyRelationshipsMedia =
    { data : List PostThingyRelationshipsMediaDataObject
    }


type alias PostThingyRelationshipsMediaDataObject =
    IdAndType


type alias PostThingyRelationshipsUser =
    { data : PostThingyRelationshipsUserData
    , links : PostThingyRelationshipsUserLinks
    }


type alias PostThingyRelationshipsUserData =
    IdAndType


type alias PostThingyRelationshipsUserLinks =
    { related : String
    }


type alias PostThingamajig =
    { attributes : PostThingamajigAttributes
    , id : String
    , relationships : PostThingamajigRelationships
    , type_ : String
    }


type alias PostThingamajigAttributes =
    { commentCount : Int
    , commenterCount : Int
    , content : String
    , createdAt : String
    , currentUserCanComment : Bool
    , currentUserCanDelete : Bool
    , currentUserCanReport : Bool
    , currentUserCanView : Bool
    , currentUserHasLiked : Bool
    , hasTiViolation : Bool
    , image : Image
    , isNewToCurrentUser : Bool
    , isPaid : Bool
    , likeCount : Int
    , metaImageUrl : String
    , moderationStatus : String
    , patreonUrl : String
    , pledgeUrl : String
    , postFile : PostThingamajigAttributesPostFile
    , postMetadata : PostThingamajigAttributesPostMetadata
    , postType : String
    , previewAssetType : String
    , publishedAt : String
    , teaserText : String
    , thumbnail : PostThingamajigAttributesThumbnail
    , title : String
    , upgradeUrl : String
    , url : String
    , wasPostedByCampaignOwner : Bool
    }


type alias PostThingamajigAttributesPostFile =
    PostFile


type alias PostThingamajigAttributesPostMetadata =
    { imageOrder : List String
    , platform : PostThingamajigAttributesPostMetadataPlatform
    }


type alias PostThingamajigAttributesPostMetadataPlatform =
    {}


type alias PostThingamajigAttributesThumbnail =
    { large : String
    , large2 : String
    , square : String
    , url : String
    }


type alias PostThingamajigRelationships =
    { accessRules : PostThingamajigRelationshipsAccessRules
    , attachmentsMedia : PostThingamajigRelationshipsAttachmentsMedia
    , audio : PostThingamajigRelationshipsAudio
    , audioPreview : PostThingamajigRelationshipsAudioPreview
    , campaign : PostThingamajigRelationshipsCampaign
    , images : PostThingamajigRelationshipsImages
    , media : PostThingamajigRelationshipsMedia
    , user : PostThingamajigRelationshipsUser
    }


type alias PostThingamajigRelationshipsAccessRules =
    { data : List PostThingamajigRelationshipsAccessRulesDataObject
    }


type alias PostThingamajigRelationshipsAccessRulesDataObject =
    IdAndType


type alias PostThingamajigRelationshipsAttachmentsMedia =
    { data : List PostThingamajigRelationshipsAttachmentsMediaDataObject
    }


type alias PostThingamajigRelationshipsAttachmentsMediaDataObject =
    IdAndType


type alias PostThingamajigRelationshipsAudio =
    { data : PostThingamajigRelationshipsAudioData
    , links : PostThingamajigRelationshipsAudioLinks
    }


type alias PostThingamajigRelationshipsAudioData =
    IdAndType


type alias PostThingamajigRelationshipsAudioLinks =
    { related : String
    }


type alias PostThingamajigRelationshipsAudioPreview =
    { data : PostThingamajigRelationshipsAudioPreviewData
    , links : PostThingamajigRelationshipsAudioPreviewLinks
    }


type alias PostThingamajigRelationshipsAudioPreviewData =
    IdAndType


type alias PostThingamajigRelationshipsAudioPreviewLinks =
    { related : String
    }


type alias PostThingamajigRelationshipsCampaign =
    { data : PostThingamajigRelationshipsCampaignData
    , links : PostThingamajigRelationshipsCampaignLinks
    }


type alias PostThingamajigRelationshipsCampaignData =
    IdAndType


type alias PostThingamajigRelationshipsCampaignLinks =
    { related : String
    }


type alias PostThingamajigRelationshipsImages =
    { data : List PostThingamajigRelationshipsImagesDataObject
    }


type alias PostThingamajigRelationshipsImagesDataObject =
    IdAndType


type alias PostThingamajigRelationshipsMedia =
    { data : List PostThingamajigRelationshipsMediaDataObject
    }


type alias PostThingamajigRelationshipsMediaDataObject =
    IdAndType


type alias PostThingamajigRelationshipsUser =
    { data : PostThingamajigRelationshipsUserData
    , links : PostThingamajigRelationshipsUserLinks
    }


type alias PostThingamajigRelationshipsUserData =
    IdAndType


type alias PostThingamajigRelationshipsUserLinks =
    { related : String
    }


type alias PostWhatsit =
    { attributes : PostWhatsitAttributes
    , id : String
    , relationships : PostWhatsitRelationships
    , type_ : String
    }


type alias PostWhatsitAttributes =
    { commentCount : Int
    , commenterCount : Int
    , content : String
    , createdAt : String
    , currentUserCanComment : Bool
    , currentUserCanDelete : Bool
    , currentUserCanReport : Bool
    , currentUserCanView : Bool
    , currentUserHasLiked : Bool
    , hasTiViolation : Bool
    , image : Image
    , isNewToCurrentUser : Bool
    , isPaid : Bool
    , likeCount : Int
    , metaImageUrl : String
    , moderationStatus : String
    , patreonUrl : String
    , pledgeUrl : String
    , postFile : PostWhatsitAttributesPostFile
    , postMetadata : PostWhatsitAttributesPostMetadata
    , postType : String
    , previewAssetType : String
    , publishedAt : String
    , teaserText : String
    , thumbnail : PostWhatsitAttributesThumbnail
    , title : String
    , upgradeUrl : String
    , url : String
    , wasPostedByCampaignOwner : Bool
    }


type alias PostWhatsitAttributesPostFile =
    { height : Int
    , imageColors : PostWhatsitAttributesPostFileImageColors
    , mediaId : Int
    , state : String
    , url : String
    , width : Int
    }


type alias PostWhatsitAttributesPostFileImageColors =
    ImageColors


type alias PostWhatsitAttributesPostMetadata =
    { imageOrder : List String
    , platform : PostWhatsitAttributesPostMetadataPlatform
    }


type alias PostWhatsitAttributesPostMetadataPlatform =
    {}


type alias PostWhatsitAttributesThumbnail =
    { large : String
    , large2 : String
    , square : String
    , url : String
    }


type alias PostWhatsitRelationships =
    { accessRules : PostWhatsitRelationshipsAccessRules
    , campaign : PostWhatsitRelationshipsCampaign
    , images : PostWhatsitRelationshipsImages
    , media : PostWhatsitRelationshipsMedia
    , user : PostWhatsitRelationshipsUser
    }


type alias PostWhatsitRelationshipsAccessRules =
    { data : List PostWhatsitRelationshipsAccessRulesDataObject
    }


type alias PostWhatsitRelationshipsAccessRulesDataObject =
    IdAndType


type alias PostWhatsitRelationshipsCampaign =
    { data : PostWhatsitRelationshipsCampaignData
    , links : PostWhatsitRelationshipsCampaignLinks
    }


type alias PostWhatsitRelationshipsCampaignData =
    IdAndType


type alias PostWhatsitRelationshipsCampaignLinks =
    { related : String
    }


type alias PostWhatsitRelationshipsImages =
    { data : List PostWhatsitRelationshipsImagesDataObject
    }


type alias PostWhatsitRelationshipsImagesDataObject =
    IdAndType


type alias PostWhatsitRelationshipsMedia =
    { data : List PostWhatsitRelationshipsMediaDataObject
    }


type alias PostWhatsitRelationshipsMediaDataObject =
    IdAndType


type alias PostWhatsitRelationshipsUser =
    { data : PostWhatsitRelationshipsUserData
    , links : PostWhatsitRelationshipsUserLinks
    }


type alias PostWhatsitRelationshipsUserData =
    IdAndType


type alias PostWhatsitRelationshipsUserLinks =
    { related : String
    }


type alias PostDoodad =
    { attributes : PostDoodadAttributes
    , id : String
    , relationships : PostDoodadRelationships
    , type_ : String
    }


type alias PostDoodadAttributes =
    { commentCount : Int
    , commenterCount : Int
    , content : String
    , createdAt : String
    , currentUserCanComment : Bool
    , currentUserCanDelete : Bool
    , currentUserCanReport : Bool
    , currentUserCanView : Bool
    , currentUserHasLiked : Bool
    , hasTiViolation : Bool
    , image : Image
    , isNewToCurrentUser : Bool
    , isPaid : Bool
    , likeCount : Int
    , metaImageUrl : String
    , minCentsPledgedToView : Int
    , moderationStatus : String
    , patreonUrl : String
    , pledgeUrl : String
    , postFile : PostDoodadAttributesPostFile
    , postMetadata : PostDoodadAttributesPostMetadata
    , postType : String
    , previewAssetType : String
    , publishedAt : String
    , teaserText : String
    , thumbnail : PostDoodadAttributesThumbnail
    , title : String
    , upgradeUrl : String
    , url : String
    , wasPostedByCampaignOwner : Bool
    }


type alias PostDoodadAttributesPostFile =
    { height : Int
    , imageColors : PostDoodadAttributesPostFileImageColors
    , mediaId : Int
    , state : String
    , url : String
    , width : Int
    }


type alias PostDoodadAttributesPostFileImageColors =
    ImageColors


type alias PostDoodadAttributesPostMetadata =
    { imageOrder : List String
    , platform : PostDoodadAttributesPostMetadataPlatform
    }


type alias PostDoodadAttributesPostMetadataPlatform =
    {}


type alias PostDoodadAttributesThumbnail =
    { large : String
    , large2 : String
    , square : String
    , url : String
    }


type alias PostDoodadRelationships =
    { accessRules : PostDoodadRelationshipsAccessRules
    , campaign : PostDoodadRelationshipsCampaign
    , images : PostDoodadRelationshipsImages
    , media : PostDoodadRelationshipsMedia
    , user : PostDoodadRelationshipsUser
    }


type alias PostDoodadRelationshipsAccessRules =
    { data : List PostDoodadRelationshipsAccessRulesDataObject
    }


type alias PostDoodadRelationshipsAccessRulesDataObject =
    IdAndType


type alias PostDoodadRelationshipsCampaign =
    { data : PostDoodadRelationshipsCampaignData
    , links : PostDoodadRelationshipsCampaignLinks
    }


type alias PostDoodadRelationshipsCampaignData =
    IdAndType


type alias PostDoodadRelationshipsCampaignLinks =
    { related : String
    }


type alias PostDoodadRelationshipsImages =
    { data : List PostDoodadRelationshipsImagesDataObject
    }


type alias PostDoodadRelationshipsImagesDataObject =
    IdAndType


type alias PostDoodadRelationshipsMedia =
    { data : List PostDoodadRelationshipsMediaDataObject
    }


type alias PostDoodadRelationshipsMediaDataObject =
    IdAndType


type alias PostDoodadRelationshipsUser =
    { data : PostDoodadRelationshipsUserData
    , links : PostDoodadRelationshipsUserLinks
    }


type alias PostDoodadRelationshipsUserData =
    IdAndType


type alias PostDoodadRelationshipsUserLinks =
    { related : String
    }


postDecoder : Json.Decode.Decoder Post
postDecoder =
    Json.Decode.oneOf
        [ Json.Decode.map Post0 postObjectDecoder
        , Json.Decode.map Post1 postMemberDecoder
        , Json.Decode.map Post2 postEntityDecoder
        , Json.Decode.map Post3 postThingDecoder
        , Json.Decode.map Post4 postInstanceDecoder
        , Json.Decode.map Post5 postConstituentDecoder
        , Json.Decode.map Post6 postSpecimenDecoder
        , Json.Decode.map Post7 postGadgetDecoder
        , Json.Decode.map Post8 postWidgetDecoder
        , Json.Decode.map Post9 postGizmoDecoder
        , Json.Decode.map Post10 postPartDecoder
        , Json.Decode.map Post11 postChunkDecoder
        , Json.Decode.map Post12 postPieceDecoder
        , Json.Decode.map Post13 postThingyDecoder
        , Json.Decode.map Post14 postThingamajigDecoder
        , Json.Decode.map Post15 postWhatsitDecoder
        , Json.Decode.map Post16 postDoodadDecoder
        ]


postObjectDecoder : Json.Decode.Decoder PostObject
postObjectDecoder =
    Json.Decode.succeed PostObject
        |> Json.Decode.Pipeline.required "attributes" postObjectAttributesDecoder
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "relationships" postObjectRelationshipsDecoder
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postObjectAttributesDecoder : Json.Decode.Decoder PostObjectAttributes
postObjectAttributesDecoder =
    Json.Decode.succeed PostObjectAttributes
        |> Json.Decode.Pipeline.required "comment_count" Json.Decode.int
        |> Json.Decode.Pipeline.required "commenter_count" Json.Decode.int
        |> Json.Decode.Pipeline.required "content" Json.Decode.string
        |> Json.Decode.Pipeline.required "created_at" Json.Decode.string
        |> Json.Decode.Pipeline.required "current_user_can_comment" Json.Decode.bool
        |> Json.Decode.Pipeline.required "current_user_can_delete" Json.Decode.bool
        |> Json.Decode.Pipeline.required "current_user_can_report" Json.Decode.bool
        |> Json.Decode.Pipeline.required "current_user_can_view" Json.Decode.bool
        |> Json.Decode.Pipeline.required "current_user_has_liked" Json.Decode.bool
        |> Json.Decode.Pipeline.required "has_ti_violation" Json.Decode.bool
        |> Json.Decode.Pipeline.required "image" imageDecoder
        |> Json.Decode.Pipeline.required "is_new_to_current_user" Json.Decode.bool
        |> Json.Decode.Pipeline.required "is_paid" Json.Decode.bool
        |> Json.Decode.Pipeline.required "like_count" Json.Decode.int
        |> Json.Decode.Pipeline.required "meta_image_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "moderation_status" Json.Decode.string
        |> Json.Decode.Pipeline.required "patreon_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "pledge_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "post_file" postFileDecoder
        |> Json.Decode.Pipeline.required "post_metadata" postObjectAttributesPostMetadataDecoder
        |> Json.Decode.Pipeline.required "post_type" Json.Decode.string
        |> Json.Decode.Pipeline.required "preview_asset_type" Json.Decode.string
        |> Json.Decode.Pipeline.required "published_at" Json.Decode.string
        |> Json.Decode.Pipeline.required "teaser_text" Json.Decode.string
        |> Json.Decode.Pipeline.required "thumbnail" postObjectAttributesThumbnailDecoder
        |> Json.Decode.Pipeline.required "title" Json.Decode.string
        |> Json.Decode.Pipeline.required "upgrade_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "url" Json.Decode.string
        |> Json.Decode.Pipeline.required "was_posted_by_campaign_owner" Json.Decode.bool


imageDecoder : Json.Decode.Decoder Image
imageDecoder =
    Json.Decode.succeed Image
        |> Json.Decode.Pipeline.required "height" Json.Decode.int
        |> Json.Decode.Pipeline.required "large_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "thumb_square_large_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "thumb_square_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "thumb_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "url" Json.Decode.string
        |> Json.Decode.Pipeline.required "width" Json.Decode.int


postFileDecoder : Json.Decode.Decoder PostFile
postFileDecoder =
    Json.Decode.succeed PostFile
        |> Json.Decode.Pipeline.required "default_thumbnail" hasUrlDecoder
        |> Json.Decode.Pipeline.required "duration" Json.Decode.int
        |> Json.Decode.Pipeline.required "full_content_duration" Json.Decode.int
        |> Json.Decode.Pipeline.required "media_id" Json.Decode.int
        |> Json.Decode.Pipeline.required "progress" progressDecoder
        |> Json.Decode.Pipeline.required "state" Json.Decode.string
        |> Json.Decode.Pipeline.required "url" Json.Decode.string


postObjectAttributesPostMetadataDecoder : Json.Decode.Decoder PostObjectAttributesPostMetadata
postObjectAttributesPostMetadataDecoder =
    Json.Decode.succeed PostObjectAttributesPostMetadata
        |> Json.Decode.Pipeline.required "image_order" (Json.Decode.list Json.Decode.string)
        |> Json.Decode.Pipeline.required "platform" postObjectAttributesPostMetadataPlatformDecoder


postObjectAttributesPostMetadataPlatformDecoder : Json.Decode.Decoder PostObjectAttributesPostMetadataPlatform
postObjectAttributesPostMetadataPlatformDecoder =
    Json.Decode.succeed PostObjectAttributesPostMetadataPlatform


postObjectAttributesThumbnailDecoder : Json.Decode.Decoder PostObjectAttributesThumbnail
postObjectAttributesThumbnailDecoder =
    Json.Decode.succeed PostObjectAttributesThumbnail
        |> Json.Decode.Pipeline.required "large" Json.Decode.string
        |> Json.Decode.Pipeline.required "large_2" Json.Decode.string
        |> Json.Decode.Pipeline.required "square" Json.Decode.string
        |> Json.Decode.Pipeline.required "url" Json.Decode.string


postObjectRelationshipsDecoder : Json.Decode.Decoder PostObjectRelationships
postObjectRelationshipsDecoder =
    Json.Decode.succeed PostObjectRelationships
        |> Json.Decode.Pipeline.required "access_rules" postObjectRelationshipsAccessRulesDecoder
        |> Json.Decode.Pipeline.required "audio" postObjectRelationshipsAudioDecoder
        |> Json.Decode.Pipeline.required "campaign" postObjectRelationshipsCampaignDecoder
        |> Json.Decode.Pipeline.required "images" postObjectRelationshipsImagesDecoder
        |> Json.Decode.Pipeline.required "media" postObjectRelationshipsMediaDecoder
        |> Json.Decode.Pipeline.required "user" postObjectRelationshipsUserDecoder


postObjectRelationshipsAccessRulesDecoder : Json.Decode.Decoder PostObjectRelationshipsAccessRules
postObjectRelationshipsAccessRulesDecoder =
    Json.Decode.succeed PostObjectRelationshipsAccessRules
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list idAndTypeDecoder)


idAndTypeDecoder : Json.Decode.Decoder IdAndType
idAndTypeDecoder =
    Json.Decode.succeed IdAndType
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postObjectRelationshipsAudioDecoder : Json.Decode.Decoder PostObjectRelationshipsAudio
postObjectRelationshipsAudioDecoder =
    Json.Decode.succeed PostObjectRelationshipsAudio
        |> Json.Decode.Pipeline.required "data" idAndTypeDecoder
        |> Json.Decode.Pipeline.required "links" postObjectRelationshipsAudioLinksDecoder


postObjectRelationshipsAudioLinksDecoder : Json.Decode.Decoder PostObjectRelationshipsAudioLinks
postObjectRelationshipsAudioLinksDecoder =
    Json.Decode.succeed PostObjectRelationshipsAudioLinks
        |> Json.Decode.Pipeline.required "related" Json.Decode.string


postObjectRelationshipsCampaignDecoder : Json.Decode.Decoder PostObjectRelationshipsCampaign
postObjectRelationshipsCampaignDecoder =
    Json.Decode.succeed PostObjectRelationshipsCampaign
        |> Json.Decode.Pipeline.required "data" idAndTypeDecoder
        |> Json.Decode.Pipeline.required "links" postObjectRelationshipsCampaignLinksDecoder


postObjectRelationshipsCampaignLinksDecoder : Json.Decode.Decoder PostObjectRelationshipsCampaignLinks
postObjectRelationshipsCampaignLinksDecoder =
    Json.Decode.succeed PostObjectRelationshipsCampaignLinks
        |> Json.Decode.Pipeline.required "related" Json.Decode.string


postObjectRelationshipsImagesDecoder : Json.Decode.Decoder PostObjectRelationshipsImages
postObjectRelationshipsImagesDecoder =
    Json.Decode.succeed PostObjectRelationshipsImages
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list idAndTypeDecoder)


postObjectRelationshipsMediaDecoder : Json.Decode.Decoder PostObjectRelationshipsMedia
postObjectRelationshipsMediaDecoder =
    Json.Decode.succeed PostObjectRelationshipsMedia
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list idAndTypeDecoder)


postObjectRelationshipsUserDecoder : Json.Decode.Decoder PostObjectRelationshipsUser
postObjectRelationshipsUserDecoder =
    Json.Decode.succeed PostObjectRelationshipsUser
        |> Json.Decode.Pipeline.required "data" idAndTypeDecoder
        |> Json.Decode.Pipeline.required "links" postObjectRelationshipsUserLinksDecoder


postObjectRelationshipsUserLinksDecoder : Json.Decode.Decoder PostObjectRelationshipsUserLinks
postObjectRelationshipsUserLinksDecoder =
    Json.Decode.succeed PostObjectRelationshipsUserLinks
        |> Json.Decode.Pipeline.required "related" Json.Decode.string


postMemberDecoder : Json.Decode.Decoder PostMember
postMemberDecoder =
    Json.Decode.succeed PostMember
        |> Json.Decode.Pipeline.required "attributes" postMemberAttributesDecoder
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "relationships" postMemberRelationshipsDecoder
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postMemberAttributesDecoder : Json.Decode.Decoder PostMemberAttributes
postMemberAttributesDecoder =
    Json.Decode.succeed PostMemberAttributes
        |> Json.Decode.Pipeline.required "comment_count" Json.Decode.int
        |> Json.Decode.Pipeline.required "commenter_count" Json.Decode.int
        |> Json.Decode.Pipeline.required "content" Json.Decode.string
        |> Json.Decode.Pipeline.required "created_at" Json.Decode.string
        |> Json.Decode.Pipeline.required "current_user_can_comment" Json.Decode.bool
        |> Json.Decode.Pipeline.required "current_user_can_delete" Json.Decode.bool
        |> Json.Decode.Pipeline.required "current_user_can_report" Json.Decode.bool
        |> Json.Decode.Pipeline.required "current_user_can_view" Json.Decode.bool
        |> Json.Decode.Pipeline.required "current_user_has_liked" Json.Decode.bool
        |> Json.Decode.Pipeline.required "embed" postMemberAttributesEmbedDecoder
        |> Json.Decode.Pipeline.required "has_ti_violation" Json.Decode.bool
        |> Json.Decode.Pipeline.required "image" imageDecoder
        |> Json.Decode.Pipeline.required "is_new_to_current_user" Json.Decode.bool
        |> Json.Decode.Pipeline.required "is_paid" Json.Decode.bool
        |> Json.Decode.Pipeline.required "like_count" Json.Decode.int
        |> Json.Decode.Pipeline.required "meta_image_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "moderation_status" Json.Decode.string
        |> Json.Decode.Pipeline.required "patreon_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "pledge_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "post_file" postMemberAttributesPostFileDecoder
        |> Json.Decode.Pipeline.required "post_metadata" postMemberAttributesPostMetadataDecoder
        |> Json.Decode.Pipeline.required "post_type" Json.Decode.string
        |> Json.Decode.Pipeline.required "preview_asset_type" Json.Decode.string
        |> Json.Decode.Pipeline.required "published_at" Json.Decode.string
        |> Json.Decode.Pipeline.required "teaser_text" Json.Decode.string
        |> Json.Decode.Pipeline.required "thumbnail" postMemberAttributesThumbnailDecoder
        |> Json.Decode.Pipeline.required "title" Json.Decode.string
        |> Json.Decode.Pipeline.required "upgrade_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "url" Json.Decode.string
        |> Json.Decode.Pipeline.required "was_posted_by_campaign_owner" Json.Decode.bool


postMemberAttributesEmbedDecoder : Json.Decode.Decoder PostMemberAttributesEmbed
postMemberAttributesEmbedDecoder =
    Json.Decode.succeed PostMemberAttributesEmbed
        |> Json.Decode.Pipeline.required "description" Json.Decode.string
        |> Json.Decode.Pipeline.required "html" Json.Decode.string
        |> Json.Decode.Pipeline.required "provider" Json.Decode.string
        |> Json.Decode.Pipeline.required "provider_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "subject" Json.Decode.string
        |> Json.Decode.Pipeline.required "url" Json.Decode.string


postMemberAttributesPostFileDecoder : Json.Decode.Decoder PostMemberAttributesPostFile
postMemberAttributesPostFileDecoder =
    Json.Decode.succeed PostMemberAttributesPostFile
        |> Json.Decode.Pipeline.required "height" Json.Decode.int
        |> Json.Decode.Pipeline.required "image_colors" imageColorsDecoder
        |> Json.Decode.Pipeline.required "media_id" Json.Decode.int
        |> Json.Decode.Pipeline.required "state" Json.Decode.string
        |> Json.Decode.Pipeline.required "url" Json.Decode.string
        |> Json.Decode.Pipeline.required "width" Json.Decode.int


imageColorsDecoder : Json.Decode.Decoder ImageColors
imageColorsDecoder =
    Json.Decode.succeed ImageColors
        |> Json.Decode.Pipeline.required "average_colors_of_corners" averageColorsOfCornersDecoder
        |> Json.Decode.Pipeline.required "dominant_color" Json.Decode.string
        |> Json.Decode.Pipeline.required "palette" (Json.Decode.list Json.Decode.string)
        |> Json.Decode.Pipeline.required "text_color" Json.Decode.string


averageColorsOfCornersDecoder : Json.Decode.Decoder AverageColorsOfCorners
averageColorsOfCornersDecoder =
    Json.Decode.succeed AverageColorsOfCorners
        |> Json.Decode.Pipeline.required "bottom_left" Json.Decode.string
        |> Json.Decode.Pipeline.required "bottom_right" Json.Decode.string
        |> Json.Decode.Pipeline.required "top_left" Json.Decode.string
        |> Json.Decode.Pipeline.required "top_right" Json.Decode.string


postMemberAttributesPostMetadataDecoder : Json.Decode.Decoder PostMemberAttributesPostMetadata
postMemberAttributesPostMetadataDecoder =
    Json.Decode.succeed PostMemberAttributesPostMetadata
        |> Json.Decode.Pipeline.required "platform" postMemberAttributesPostMetadataPlatformDecoder


postMemberAttributesPostMetadataPlatformDecoder : Json.Decode.Decoder PostMemberAttributesPostMetadataPlatform
postMemberAttributesPostMetadataPlatformDecoder =
    Json.Decode.succeed PostMemberAttributesPostMetadataPlatform


postMemberAttributesThumbnailDecoder : Json.Decode.Decoder PostMemberAttributesThumbnail
postMemberAttributesThumbnailDecoder =
    Json.Decode.succeed PostMemberAttributesThumbnail
        |> Json.Decode.Pipeline.required "large" Json.Decode.string
        |> Json.Decode.Pipeline.required "large_2" Json.Decode.string
        |> Json.Decode.Pipeline.required "square" Json.Decode.string
        |> Json.Decode.Pipeline.required "url" Json.Decode.string


postMemberRelationshipsDecoder : Json.Decode.Decoder PostMemberRelationships
postMemberRelationshipsDecoder =
    Json.Decode.succeed PostMemberRelationships
        |> Json.Decode.Pipeline.required "access_rules" postMemberRelationshipsAccessRulesDecoder
        |> Json.Decode.Pipeline.required "campaign" postMemberRelationshipsCampaignDecoder
        |> Json.Decode.Pipeline.required "images" postMemberRelationshipsImagesDecoder
        |> Json.Decode.Pipeline.required "media" postMemberRelationshipsMediaDecoder
        |> Json.Decode.Pipeline.required "user" postMemberRelationshipsUserDecoder


postMemberRelationshipsAccessRulesDecoder : Json.Decode.Decoder PostMemberRelationshipsAccessRules
postMemberRelationshipsAccessRulesDecoder =
    Json.Decode.succeed PostMemberRelationshipsAccessRules
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list idAndTypeDecoder)


postMemberRelationshipsCampaignDecoder : Json.Decode.Decoder PostMemberRelationshipsCampaign
postMemberRelationshipsCampaignDecoder =
    Json.Decode.succeed PostMemberRelationshipsCampaign
        |> Json.Decode.Pipeline.required "data" idAndTypeDecoder
        |> Json.Decode.Pipeline.required "links" postMemberRelationshipsCampaignLinksDecoder


postMemberRelationshipsCampaignLinksDecoder : Json.Decode.Decoder PostMemberRelationshipsCampaignLinks
postMemberRelationshipsCampaignLinksDecoder =
    Json.Decode.succeed PostMemberRelationshipsCampaignLinks
        |> Json.Decode.Pipeline.required "related" Json.Decode.string


postMemberRelationshipsImagesDecoder : Json.Decode.Decoder PostMemberRelationshipsImages
postMemberRelationshipsImagesDecoder =
    Json.Decode.succeed PostMemberRelationshipsImages
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list idAndTypeDecoder)


postMemberRelationshipsMediaDecoder : Json.Decode.Decoder PostMemberRelationshipsMedia
postMemberRelationshipsMediaDecoder =
    Json.Decode.succeed PostMemberRelationshipsMedia
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list idAndTypeDecoder)


postMemberRelationshipsUserDecoder : Json.Decode.Decoder PostMemberRelationshipsUser
postMemberRelationshipsUserDecoder =
    Json.Decode.succeed PostMemberRelationshipsUser
        |> Json.Decode.Pipeline.required "data" idAndTypeDecoder
        |> Json.Decode.Pipeline.required "links" postMemberRelationshipsUserLinksDecoder


postMemberRelationshipsUserLinksDecoder : Json.Decode.Decoder PostMemberRelationshipsUserLinks
postMemberRelationshipsUserLinksDecoder =
    Json.Decode.succeed PostMemberRelationshipsUserLinks
        |> Json.Decode.Pipeline.required "related" Json.Decode.string


postEntityDecoder : Json.Decode.Decoder PostEntity
postEntityDecoder =
    Json.Decode.succeed PostEntity
        |> Json.Decode.Pipeline.required "attributes" postEntityAttributesDecoder
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "relationships" postEntityRelationshipsDecoder
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postEntityAttributesDecoder : Json.Decode.Decoder PostEntityAttributes
postEntityAttributesDecoder =
    Json.Decode.succeed PostEntityAttributes
        |> Json.Decode.Pipeline.required "comment_count" Json.Decode.int
        |> Json.Decode.Pipeline.required "commenter_count" Json.Decode.int
        |> Json.Decode.Pipeline.required "content" Json.Decode.string
        |> Json.Decode.Pipeline.required "created_at" Json.Decode.string
        |> Json.Decode.Pipeline.required "current_user_can_comment" Json.Decode.bool
        |> Json.Decode.Pipeline.required "current_user_can_delete" Json.Decode.bool
        |> Json.Decode.Pipeline.required "current_user_can_report" Json.Decode.bool
        |> Json.Decode.Pipeline.required "current_user_can_view" Json.Decode.bool
        |> Json.Decode.Pipeline.required "current_user_has_liked" Json.Decode.bool
        |> Json.Decode.Pipeline.required "has_ti_violation" Json.Decode.bool
        |> Json.Decode.Pipeline.required "is_new_to_current_user" Json.Decode.bool
        |> Json.Decode.Pipeline.required "is_paid" Json.Decode.bool
        |> Json.Decode.Pipeline.required "like_count" Json.Decode.int
        |> Json.Decode.Pipeline.required "meta_image_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "moderation_status" Json.Decode.string
        |> Json.Decode.Pipeline.required "patreon_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "pledge_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "post_type" Json.Decode.string
        |> Json.Decode.Pipeline.required "published_at" Json.Decode.string
        |> Json.Decode.Pipeline.required "title" Json.Decode.string
        |> Json.Decode.Pipeline.required "upgrade_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "url" Json.Decode.string
        |> Json.Decode.Pipeline.required "was_posted_by_campaign_owner" Json.Decode.bool


postEntityRelationshipsDecoder : Json.Decode.Decoder PostEntityRelationships
postEntityRelationshipsDecoder =
    Json.Decode.succeed PostEntityRelationships
        |> Json.Decode.Pipeline.required "access_rules" postEntityRelationshipsAccessRulesDecoder
        |> Json.Decode.Pipeline.required "campaign" postEntityRelationshipsCampaignDecoder
        |> Json.Decode.Pipeline.required "user" postEntityRelationshipsUserDecoder


postEntityRelationshipsAccessRulesDecoder : Json.Decode.Decoder PostEntityRelationshipsAccessRules
postEntityRelationshipsAccessRulesDecoder =
    Json.Decode.succeed PostEntityRelationshipsAccessRules
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list idAndTypeDecoder)


postEntityRelationshipsCampaignDecoder : Json.Decode.Decoder PostEntityRelationshipsCampaign
postEntityRelationshipsCampaignDecoder =
    Json.Decode.succeed PostEntityRelationshipsCampaign
        |> Json.Decode.Pipeline.required "data" idAndTypeDecoder
        |> Json.Decode.Pipeline.required "links" postEntityRelationshipsCampaignLinksDecoder


postEntityRelationshipsCampaignLinksDecoder : Json.Decode.Decoder PostEntityRelationshipsCampaignLinks
postEntityRelationshipsCampaignLinksDecoder =
    Json.Decode.succeed PostEntityRelationshipsCampaignLinks
        |> Json.Decode.Pipeline.required "related" Json.Decode.string


postEntityRelationshipsUserDecoder : Json.Decode.Decoder PostEntityRelationshipsUser
postEntityRelationshipsUserDecoder =
    Json.Decode.succeed PostEntityRelationshipsUser
        |> Json.Decode.Pipeline.required "data" idAndTypeDecoder
        |> Json.Decode.Pipeline.required "links" postEntityRelationshipsUserLinksDecoder


postEntityRelationshipsUserLinksDecoder : Json.Decode.Decoder PostEntityRelationshipsUserLinks
postEntityRelationshipsUserLinksDecoder =
    Json.Decode.succeed PostEntityRelationshipsUserLinks
        |> Json.Decode.Pipeline.required "related" Json.Decode.string


postThingDecoder : Json.Decode.Decoder PostThing
postThingDecoder =
    Json.Decode.succeed PostThing
        |> Json.Decode.Pipeline.required "attributes" postThingAttributesDecoder
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "relationships" postThingRelationshipsDecoder
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postThingAttributesDecoder : Json.Decode.Decoder PostThingAttributes
postThingAttributesDecoder =
    Json.Decode.succeed PostThingAttributes
        |> Json.Decode.Pipeline.required "comment_count" Json.Decode.int
        |> Json.Decode.Pipeline.required "commenter_count" Json.Decode.int
        |> Json.Decode.Pipeline.required "content" Json.Decode.string
        |> Json.Decode.Pipeline.required "created_at" Json.Decode.string
        |> Json.Decode.Pipeline.required "current_user_can_comment" Json.Decode.bool
        |> Json.Decode.Pipeline.required "current_user_can_delete" Json.Decode.bool
        |> Json.Decode.Pipeline.required "current_user_can_report" Json.Decode.bool
        |> Json.Decode.Pipeline.required "current_user_can_view" Json.Decode.bool
        |> Json.Decode.Pipeline.required "current_user_has_liked" Json.Decode.bool
        |> Json.Decode.Pipeline.required "has_ti_violation" Json.Decode.bool
        |> Json.Decode.Pipeline.required "image" imageDecoder
        |> Json.Decode.Pipeline.required "is_new_to_current_user" Json.Decode.bool
        |> Json.Decode.Pipeline.required "is_paid" Json.Decode.bool
        |> Json.Decode.Pipeline.required "like_count" Json.Decode.int
        |> Json.Decode.Pipeline.required "meta_image_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "moderation_status" Json.Decode.string
        |> Json.Decode.Pipeline.required "patreon_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "pledge_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "post_file" postFileDecoder
        |> Json.Decode.Pipeline.required "post_metadata" postThingAttributesPostMetadataDecoder
        |> Json.Decode.Pipeline.required "post_type" Json.Decode.string
        |> Json.Decode.Pipeline.required "preview_asset_type" Json.Decode.string
        |> Json.Decode.Pipeline.required "published_at" Json.Decode.string
        |> Json.Decode.Pipeline.required "teaser_text" Json.Decode.string
        |> Json.Decode.Pipeline.required "thumbnail" postThingAttributesThumbnailDecoder
        |> Json.Decode.Pipeline.required "title" Json.Decode.string
        |> Json.Decode.Pipeline.required "upgrade_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "url" Json.Decode.string
        |> Json.Decode.Pipeline.required "was_posted_by_campaign_owner" Json.Decode.bool


hasUrlDecoder : Json.Decode.Decoder HasUrl
hasUrlDecoder =
    Json.Decode.succeed HasUrl
        |> Json.Decode.Pipeline.required "url" Json.Decode.string


progressDecoder : Json.Decode.Decoder Progress
progressDecoder =
    Json.Decode.succeed Progress
        |> Json.Decode.Pipeline.required "is_watched" Json.Decode.bool
        |> Json.Decode.Pipeline.required "watch_state" Json.Decode.string


postThingAttributesPostMetadataDecoder : Json.Decode.Decoder PostThingAttributesPostMetadata
postThingAttributesPostMetadataDecoder =
    Json.Decode.succeed PostThingAttributesPostMetadata
        |> Json.Decode.Pipeline.required "image_order" (Json.Decode.list Json.Decode.string)
        |> Json.Decode.Pipeline.required "platform" postThingAttributesPostMetadataPlatformDecoder


postThingAttributesPostMetadataPlatformDecoder : Json.Decode.Decoder PostThingAttributesPostMetadataPlatform
postThingAttributesPostMetadataPlatformDecoder =
    Json.Decode.succeed PostThingAttributesPostMetadataPlatform


postThingAttributesThumbnailDecoder : Json.Decode.Decoder PostThingAttributesThumbnail
postThingAttributesThumbnailDecoder =
    Json.Decode.succeed PostThingAttributesThumbnail
        |> Json.Decode.Pipeline.required "large" Json.Decode.string
        |> Json.Decode.Pipeline.required "large_2" Json.Decode.string
        |> Json.Decode.Pipeline.required "square" Json.Decode.string
        |> Json.Decode.Pipeline.required "url" Json.Decode.string


postThingRelationshipsDecoder : Json.Decode.Decoder PostThingRelationships
postThingRelationshipsDecoder =
    Json.Decode.succeed PostThingRelationships
        |> Json.Decode.Pipeline.required "access_rules" postThingRelationshipsAccessRulesDecoder
        |> Json.Decode.Pipeline.required "attachments_media" postThingRelationshipsAttachmentsMediaDecoder
        |> Json.Decode.Pipeline.required "audio" postThingRelationshipsAudioDecoder
        |> Json.Decode.Pipeline.required "campaign" postThingRelationshipsCampaignDecoder
        |> Json.Decode.Pipeline.required "images" postThingRelationshipsImagesDecoder
        |> Json.Decode.Pipeline.required "media" postThingRelationshipsMediaDecoder
        |> Json.Decode.Pipeline.required "user" postThingRelationshipsUserDecoder


postThingRelationshipsAccessRulesDecoder : Json.Decode.Decoder PostThingRelationshipsAccessRules
postThingRelationshipsAccessRulesDecoder =
    Json.Decode.succeed PostThingRelationshipsAccessRules
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list idAndTypeDecoder)


postThingRelationshipsAttachmentsMediaDecoder : Json.Decode.Decoder PostThingRelationshipsAttachmentsMedia
postThingRelationshipsAttachmentsMediaDecoder =
    Json.Decode.succeed PostThingRelationshipsAttachmentsMedia
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list idAndTypeDecoder)


postThingRelationshipsAudioDecoder : Json.Decode.Decoder PostThingRelationshipsAudio
postThingRelationshipsAudioDecoder =
    Json.Decode.succeed PostThingRelationshipsAudio
        |> Json.Decode.Pipeline.required "data" idAndTypeDecoder
        |> Json.Decode.Pipeline.required "links" postThingRelationshipsAudioLinksDecoder


postThingRelationshipsAudioLinksDecoder : Json.Decode.Decoder PostThingRelationshipsAudioLinks
postThingRelationshipsAudioLinksDecoder =
    Json.Decode.succeed PostThingRelationshipsAudioLinks
        |> Json.Decode.Pipeline.required "related" Json.Decode.string


postThingRelationshipsCampaignDecoder : Json.Decode.Decoder PostThingRelationshipsCampaign
postThingRelationshipsCampaignDecoder =
    Json.Decode.succeed PostThingRelationshipsCampaign
        |> Json.Decode.Pipeline.required "data" idAndTypeDecoder
        |> Json.Decode.Pipeline.required "links" postThingRelationshipsCampaignLinksDecoder


postThingRelationshipsCampaignLinksDecoder : Json.Decode.Decoder PostThingRelationshipsCampaignLinks
postThingRelationshipsCampaignLinksDecoder =
    Json.Decode.succeed PostThingRelationshipsCampaignLinks
        |> Json.Decode.Pipeline.required "related" Json.Decode.string


postThingRelationshipsImagesDecoder : Json.Decode.Decoder PostThingRelationshipsImages
postThingRelationshipsImagesDecoder =
    Json.Decode.succeed PostThingRelationshipsImages
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list idAndTypeDecoder)


postThingRelationshipsMediaDecoder : Json.Decode.Decoder PostThingRelationshipsMedia
postThingRelationshipsMediaDecoder =
    Json.Decode.succeed PostThingRelationshipsMedia
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list idAndTypeDecoder)


postThingRelationshipsUserDecoder : Json.Decode.Decoder PostThingRelationshipsUser
postThingRelationshipsUserDecoder =
    Json.Decode.succeed PostThingRelationshipsUser
        |> Json.Decode.Pipeline.required "data" idAndTypeDecoder
        |> Json.Decode.Pipeline.required "links" postThingRelationshipsUserLinksDecoder


postThingRelationshipsUserLinksDecoder : Json.Decode.Decoder PostThingRelationshipsUserLinks
postThingRelationshipsUserLinksDecoder =
    Json.Decode.succeed PostThingRelationshipsUserLinks
        |> Json.Decode.Pipeline.required "related" Json.Decode.string


postInstanceDecoder : Json.Decode.Decoder PostInstance
postInstanceDecoder =
    Json.Decode.succeed PostInstance
        |> Json.Decode.Pipeline.required "attributes" postInstanceAttributesDecoder
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "relationships" postInstanceRelationshipsDecoder
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postInstanceAttributesDecoder : Json.Decode.Decoder PostInstanceAttributes
postInstanceAttributesDecoder =
    Json.Decode.succeed PostInstanceAttributes
        |> Json.Decode.Pipeline.required "comment_count" Json.Decode.int
        |> Json.Decode.Pipeline.required "commenter_count" Json.Decode.int
        |> Json.Decode.Pipeline.required "content" Json.Decode.string
        |> Json.Decode.Pipeline.required "created_at" Json.Decode.string
        |> Json.Decode.Pipeline.required "current_user_can_comment" Json.Decode.bool
        |> Json.Decode.Pipeline.required "current_user_can_delete" Json.Decode.bool
        |> Json.Decode.Pipeline.required "current_user_can_report" Json.Decode.bool
        |> Json.Decode.Pipeline.required "current_user_can_view" Json.Decode.bool
        |> Json.Decode.Pipeline.required "current_user_has_liked" Json.Decode.bool
        |> Json.Decode.Pipeline.required "has_ti_violation" Json.Decode.bool
        |> Json.Decode.Pipeline.required "image" imageDecoder
        |> Json.Decode.Pipeline.required "is_new_to_current_user" Json.Decode.bool
        |> Json.Decode.Pipeline.required "is_paid" Json.Decode.bool
        |> Json.Decode.Pipeline.required "like_count" Json.Decode.int
        |> Json.Decode.Pipeline.required "meta_image_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "moderation_status" Json.Decode.string
        |> Json.Decode.Pipeline.required "patreon_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "pledge_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "post_file" postInstanceAttributesPostFileDecoder
        |> Json.Decode.Pipeline.required "post_metadata" postInstanceAttributesPostMetadataDecoder
        |> Json.Decode.Pipeline.required "post_type" Json.Decode.string
        |> Json.Decode.Pipeline.required "preview_asset_type" Json.Decode.string
        |> Json.Decode.Pipeline.required "published_at" Json.Decode.string
        |> Json.Decode.Pipeline.required "teaser_text" Json.Decode.string
        |> Json.Decode.Pipeline.required "thumbnail" postInstanceAttributesThumbnailDecoder
        |> Json.Decode.Pipeline.required "title" Json.Decode.string
        |> Json.Decode.Pipeline.required "upgrade_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "url" Json.Decode.string
        |> Json.Decode.Pipeline.required "was_posted_by_campaign_owner" Json.Decode.bool


postInstanceAttributesPostFileDecoder : Json.Decode.Decoder PostInstanceAttributesPostFile
postInstanceAttributesPostFileDecoder =
    Json.Decode.succeed PostInstanceAttributesPostFile
        |> Json.Decode.Pipeline.required "height" Json.Decode.int
        |> Json.Decode.Pipeline.required "image_colors" imageColorsDecoder
        |> Json.Decode.Pipeline.required "media_id" Json.Decode.int
        |> Json.Decode.Pipeline.required "state" Json.Decode.string
        |> Json.Decode.Pipeline.required "url" Json.Decode.string
        |> Json.Decode.Pipeline.required "width" Json.Decode.int


postInstanceAttributesPostMetadataDecoder : Json.Decode.Decoder PostInstanceAttributesPostMetadata
postInstanceAttributesPostMetadataDecoder =
    Json.Decode.succeed PostInstanceAttributesPostMetadata
        |> Json.Decode.Pipeline.required "image_order" (Json.Decode.list Json.Decode.string)
        |> Json.Decode.Pipeline.required "platform" postInstanceAttributesPostMetadataPlatformDecoder


postInstanceAttributesPostMetadataPlatformDecoder : Json.Decode.Decoder PostInstanceAttributesPostMetadataPlatform
postInstanceAttributesPostMetadataPlatformDecoder =
    Json.Decode.succeed PostInstanceAttributesPostMetadataPlatform


postInstanceAttributesThumbnailDecoder : Json.Decode.Decoder PostInstanceAttributesThumbnail
postInstanceAttributesThumbnailDecoder =
    Json.Decode.succeed PostInstanceAttributesThumbnail
        |> Json.Decode.Pipeline.required "large" Json.Decode.string
        |> Json.Decode.Pipeline.required "large_2" Json.Decode.string
        |> Json.Decode.Pipeline.required "square" Json.Decode.string
        |> Json.Decode.Pipeline.required "url" Json.Decode.string


postInstanceRelationshipsDecoder : Json.Decode.Decoder PostInstanceRelationships
postInstanceRelationshipsDecoder =
    Json.Decode.succeed PostInstanceRelationships
        |> Json.Decode.Pipeline.required "access_rules" postInstanceRelationshipsAccessRulesDecoder
        |> Json.Decode.Pipeline.required "campaign" postInstanceRelationshipsCampaignDecoder
        |> Json.Decode.Pipeline.required "images" postInstanceRelationshipsImagesDecoder
        |> Json.Decode.Pipeline.required "media" postInstanceRelationshipsMediaDecoder
        |> Json.Decode.Pipeline.required "user" postInstanceRelationshipsUserDecoder


postInstanceRelationshipsAccessRulesDecoder : Json.Decode.Decoder PostInstanceRelationshipsAccessRules
postInstanceRelationshipsAccessRulesDecoder =
    Json.Decode.succeed PostInstanceRelationshipsAccessRules
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list idAndTypeDecoder)


postInstanceRelationshipsCampaignDecoder : Json.Decode.Decoder PostInstanceRelationshipsCampaign
postInstanceRelationshipsCampaignDecoder =
    Json.Decode.succeed PostInstanceRelationshipsCampaign
        |> Json.Decode.Pipeline.required "data" idAndTypeDecoder
        |> Json.Decode.Pipeline.required "links" postInstanceRelationshipsCampaignLinksDecoder


postInstanceRelationshipsCampaignLinksDecoder : Json.Decode.Decoder PostInstanceRelationshipsCampaignLinks
postInstanceRelationshipsCampaignLinksDecoder =
    Json.Decode.succeed PostInstanceRelationshipsCampaignLinks
        |> Json.Decode.Pipeline.required "related" Json.Decode.string


postInstanceRelationshipsImagesDecoder : Json.Decode.Decoder PostInstanceRelationshipsImages
postInstanceRelationshipsImagesDecoder =
    Json.Decode.succeed PostInstanceRelationshipsImages
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list idAndTypeDecoder)


postInstanceRelationshipsMediaDecoder : Json.Decode.Decoder PostInstanceRelationshipsMedia
postInstanceRelationshipsMediaDecoder =
    Json.Decode.succeed PostInstanceRelationshipsMedia
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list idAndTypeDecoder)


postInstanceRelationshipsUserDecoder : Json.Decode.Decoder PostInstanceRelationshipsUser
postInstanceRelationshipsUserDecoder =
    Json.Decode.succeed PostInstanceRelationshipsUser
        |> Json.Decode.Pipeline.required "data" idAndTypeDecoder
        |> Json.Decode.Pipeline.required "links" postInstanceRelationshipsUserLinksDecoder


postInstanceRelationshipsUserLinksDecoder : Json.Decode.Decoder PostInstanceRelationshipsUserLinks
postInstanceRelationshipsUserLinksDecoder =
    Json.Decode.succeed PostInstanceRelationshipsUserLinks
        |> Json.Decode.Pipeline.required "related" Json.Decode.string


postConstituentDecoder : Json.Decode.Decoder PostConstituent
postConstituentDecoder =
    Json.Decode.succeed PostConstituent
        |> Json.Decode.Pipeline.required "attributes" postConstituentAttributesDecoder
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "relationships" postConstituentRelationshipsDecoder
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postConstituentAttributesDecoder : Json.Decode.Decoder PostConstituentAttributes
postConstituentAttributesDecoder =
    Json.Decode.succeed PostConstituentAttributes
        |> Json.Decode.Pipeline.required "comment_count" Json.Decode.int
        |> Json.Decode.Pipeline.required "commenter_count" Json.Decode.int
        |> Json.Decode.Pipeline.required "content" Json.Decode.string
        |> Json.Decode.Pipeline.required "created_at" Json.Decode.string
        |> Json.Decode.Pipeline.required "current_user_can_comment" Json.Decode.bool
        |> Json.Decode.Pipeline.required "current_user_can_delete" Json.Decode.bool
        |> Json.Decode.Pipeline.required "current_user_can_report" Json.Decode.bool
        |> Json.Decode.Pipeline.required "current_user_can_view" Json.Decode.bool
        |> Json.Decode.Pipeline.required "current_user_has_liked" Json.Decode.bool
        |> Json.Decode.Pipeline.required "has_ti_violation" Json.Decode.bool
        |> Json.Decode.Pipeline.required "image" imageDecoder
        |> Json.Decode.Pipeline.required "is_new_to_current_user" Json.Decode.bool
        |> Json.Decode.Pipeline.required "is_paid" Json.Decode.bool
        |> Json.Decode.Pipeline.required "like_count" Json.Decode.int
        |> Json.Decode.Pipeline.required "meta_image_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "moderation_status" Json.Decode.string
        |> Json.Decode.Pipeline.required "patreon_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "pledge_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "post_file" postConstituentAttributesPostFileDecoder
        |> Json.Decode.Pipeline.required "post_metadata" postConstituentAttributesPostMetadataDecoder
        |> Json.Decode.Pipeline.required "post_type" Json.Decode.string
        |> Json.Decode.Pipeline.required "published_at" Json.Decode.string
        |> Json.Decode.Pipeline.required "thumbnail" postConstituentAttributesThumbnailDecoder
        |> Json.Decode.Pipeline.required "title" Json.Decode.string
        |> Json.Decode.Pipeline.required "upgrade_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "url" Json.Decode.string
        |> Json.Decode.Pipeline.required "was_posted_by_campaign_owner" Json.Decode.bool


postConstituentAttributesPostFileDecoder : Json.Decode.Decoder PostConstituentAttributesPostFile
postConstituentAttributesPostFileDecoder =
    Json.Decode.succeed PostConstituentAttributesPostFile
        |> Json.Decode.Pipeline.required "height" Json.Decode.int
        |> Json.Decode.Pipeline.required "image_colors" imageColorsDecoder
        |> Json.Decode.Pipeline.required "media_id" Json.Decode.int
        |> Json.Decode.Pipeline.required "state" Json.Decode.string
        |> Json.Decode.Pipeline.required "url" Json.Decode.string
        |> Json.Decode.Pipeline.required "width" Json.Decode.int


postConstituentAttributesPostMetadataDecoder : Json.Decode.Decoder PostConstituentAttributesPostMetadata
postConstituentAttributesPostMetadataDecoder =
    Json.Decode.succeed PostConstituentAttributesPostMetadata
        |> Json.Decode.Pipeline.required "image_order" (Json.Decode.list Json.Decode.string)


postConstituentAttributesThumbnailDecoder : Json.Decode.Decoder PostConstituentAttributesThumbnail
postConstituentAttributesThumbnailDecoder =
    Json.Decode.succeed PostConstituentAttributesThumbnail
        |> Json.Decode.Pipeline.required "large" Json.Decode.string
        |> Json.Decode.Pipeline.required "large_2" Json.Decode.string
        |> Json.Decode.Pipeline.required "square" Json.Decode.string
        |> Json.Decode.Pipeline.required "url" Json.Decode.string


postConstituentRelationshipsDecoder : Json.Decode.Decoder PostConstituentRelationships
postConstituentRelationshipsDecoder =
    Json.Decode.succeed PostConstituentRelationships
        |> Json.Decode.Pipeline.required "access_rules" postConstituentRelationshipsAccessRulesDecoder
        |> Json.Decode.Pipeline.required "campaign" postConstituentRelationshipsCampaignDecoder
        |> Json.Decode.Pipeline.required "images" postConstituentRelationshipsImagesDecoder
        |> Json.Decode.Pipeline.required "media" postConstituentRelationshipsMediaDecoder
        |> Json.Decode.Pipeline.required "user" postConstituentRelationshipsUserDecoder


postConstituentRelationshipsAccessRulesDecoder : Json.Decode.Decoder PostConstituentRelationshipsAccessRules
postConstituentRelationshipsAccessRulesDecoder =
    Json.Decode.succeed PostConstituentRelationshipsAccessRules
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list idAndTypeDecoder)


postConstituentRelationshipsCampaignDecoder : Json.Decode.Decoder PostConstituentRelationshipsCampaign
postConstituentRelationshipsCampaignDecoder =
    Json.Decode.succeed PostConstituentRelationshipsCampaign
        |> Json.Decode.Pipeline.required "data" idAndTypeDecoder
        |> Json.Decode.Pipeline.required "links" postConstituentRelationshipsCampaignLinksDecoder


postConstituentRelationshipsCampaignLinksDecoder : Json.Decode.Decoder PostConstituentRelationshipsCampaignLinks
postConstituentRelationshipsCampaignLinksDecoder =
    Json.Decode.succeed PostConstituentRelationshipsCampaignLinks
        |> Json.Decode.Pipeline.required "related" Json.Decode.string


postConstituentRelationshipsImagesDecoder : Json.Decode.Decoder PostConstituentRelationshipsImages
postConstituentRelationshipsImagesDecoder =
    Json.Decode.succeed PostConstituentRelationshipsImages
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list idAndTypeDecoder)


postConstituentRelationshipsMediaDecoder : Json.Decode.Decoder PostConstituentRelationshipsMedia
postConstituentRelationshipsMediaDecoder =
    Json.Decode.succeed PostConstituentRelationshipsMedia
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list idAndTypeDecoder)


postConstituentRelationshipsUserDecoder : Json.Decode.Decoder PostConstituentRelationshipsUser
postConstituentRelationshipsUserDecoder =
    Json.Decode.succeed PostConstituentRelationshipsUser
        |> Json.Decode.Pipeline.required "data" idAndTypeDecoder
        |> Json.Decode.Pipeline.required "links" postConstituentRelationshipsUserLinksDecoder


postConstituentRelationshipsUserLinksDecoder : Json.Decode.Decoder PostConstituentRelationshipsUserLinks
postConstituentRelationshipsUserLinksDecoder =
    Json.Decode.succeed PostConstituentRelationshipsUserLinks
        |> Json.Decode.Pipeline.required "related" Json.Decode.string


postSpecimenDecoder : Json.Decode.Decoder PostSpecimen
postSpecimenDecoder =
    Json.Decode.succeed PostSpecimen
        |> Json.Decode.Pipeline.required "attributes" postSpecimenAttributesDecoder
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "relationships" postSpecimenRelationshipsDecoder
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postSpecimenAttributesDecoder : Json.Decode.Decoder PostSpecimenAttributes
postSpecimenAttributesDecoder =
    Json.Decode.succeed PostSpecimenAttributes
        |> Json.Decode.Pipeline.required "comment_count" Json.Decode.int
        |> Json.Decode.Pipeline.required "commenter_count" Json.Decode.int
        |> Json.Decode.Pipeline.required "content" Json.Decode.string
        |> Json.Decode.Pipeline.required "created_at" Json.Decode.string
        |> Json.Decode.Pipeline.required "current_user_can_comment" Json.Decode.bool
        |> Json.Decode.Pipeline.required "current_user_can_delete" Json.Decode.bool
        |> Json.Decode.Pipeline.required "current_user_can_report" Json.Decode.bool
        |> Json.Decode.Pipeline.required "current_user_can_view" Json.Decode.bool
        |> Json.Decode.Pipeline.required "current_user_has_liked" Json.Decode.bool
        |> Json.Decode.Pipeline.required "embed" postSpecimenAttributesEmbedDecoder
        |> Json.Decode.Pipeline.required "has_ti_violation" Json.Decode.bool
        |> Json.Decode.Pipeline.required "image" imageDecoder
        |> Json.Decode.Pipeline.required "is_new_to_current_user" Json.Decode.bool
        |> Json.Decode.Pipeline.required "is_paid" Json.Decode.bool
        |> Json.Decode.Pipeline.required "like_count" Json.Decode.int
        |> Json.Decode.Pipeline.required "meta_image_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "moderation_status" Json.Decode.string
        |> Json.Decode.Pipeline.required "patreon_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "pledge_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "post_file" postSpecimenAttributesPostFileDecoder
        |> Json.Decode.Pipeline.required "post_metadata" postSpecimenAttributesPostMetadataDecoder
        |> Json.Decode.Pipeline.required "post_type" Json.Decode.string
        |> Json.Decode.Pipeline.required "preview_asset_type" Json.Decode.string
        |> Json.Decode.Pipeline.required "published_at" Json.Decode.string
        |> Json.Decode.Pipeline.required "teaser_text" Json.Decode.string
        |> Json.Decode.Pipeline.required "thumbnail" postSpecimenAttributesThumbnailDecoder
        |> Json.Decode.Pipeline.required "title" Json.Decode.string
        |> Json.Decode.Pipeline.required "upgrade_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "url" Json.Decode.string
        |> Json.Decode.Pipeline.required "was_posted_by_campaign_owner" Json.Decode.bool


postSpecimenAttributesEmbedDecoder : Json.Decode.Decoder PostSpecimenAttributesEmbed
postSpecimenAttributesEmbedDecoder =
    Json.Decode.succeed PostSpecimenAttributesEmbed
        |> Json.Decode.Pipeline.required "description" Json.Decode.string
        |> Json.Decode.Pipeline.required "provider" Json.Decode.string
        |> Json.Decode.Pipeline.required "provider_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "subject" Json.Decode.string
        |> Json.Decode.Pipeline.required "url" Json.Decode.string


postSpecimenAttributesPostFileDecoder : Json.Decode.Decoder PostSpecimenAttributesPostFile
postSpecimenAttributesPostFileDecoder =
    Json.Decode.succeed PostSpecimenAttributesPostFile
        |> Json.Decode.Pipeline.required "height" Json.Decode.int
        |> Json.Decode.Pipeline.required "image_colors" imageColorsDecoder
        |> Json.Decode.Pipeline.required "media_id" Json.Decode.int
        |> Json.Decode.Pipeline.required "state" Json.Decode.string
        |> Json.Decode.Pipeline.required "url" Json.Decode.string
        |> Json.Decode.Pipeline.required "width" Json.Decode.int


postSpecimenAttributesPostMetadataDecoder : Json.Decode.Decoder PostSpecimenAttributesPostMetadata
postSpecimenAttributesPostMetadataDecoder =
    Json.Decode.succeed PostSpecimenAttributesPostMetadata
        |> Json.Decode.Pipeline.required "platform" postSpecimenAttributesPostMetadataPlatformDecoder


postSpecimenAttributesPostMetadataPlatformDecoder : Json.Decode.Decoder PostSpecimenAttributesPostMetadataPlatform
postSpecimenAttributesPostMetadataPlatformDecoder =
    Json.Decode.succeed PostSpecimenAttributesPostMetadataPlatform


postSpecimenAttributesThumbnailDecoder : Json.Decode.Decoder PostSpecimenAttributesThumbnail
postSpecimenAttributesThumbnailDecoder =
    Json.Decode.succeed PostSpecimenAttributesThumbnail
        |> Json.Decode.Pipeline.required "large" Json.Decode.string
        |> Json.Decode.Pipeline.required "large_2" Json.Decode.string
        |> Json.Decode.Pipeline.required "square" Json.Decode.string
        |> Json.Decode.Pipeline.required "url" Json.Decode.string


postSpecimenRelationshipsDecoder : Json.Decode.Decoder PostSpecimenRelationships
postSpecimenRelationshipsDecoder =
    Json.Decode.succeed PostSpecimenRelationships
        |> Json.Decode.Pipeline.required "access_rules" postSpecimenRelationshipsAccessRulesDecoder
        |> Json.Decode.Pipeline.required "campaign" postSpecimenRelationshipsCampaignDecoder
        |> Json.Decode.Pipeline.required "images" postSpecimenRelationshipsImagesDecoder
        |> Json.Decode.Pipeline.required "media" postSpecimenRelationshipsMediaDecoder
        |> Json.Decode.Pipeline.required "user" postSpecimenRelationshipsUserDecoder
        |> Json.Decode.Pipeline.required "user_defined_tags" postSpecimenRelationshipsUserDefinedTagsDecoder


postSpecimenRelationshipsAccessRulesDecoder : Json.Decode.Decoder PostSpecimenRelationshipsAccessRules
postSpecimenRelationshipsAccessRulesDecoder =
    Json.Decode.succeed PostSpecimenRelationshipsAccessRules
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list idAndTypeDecoder)


postSpecimenRelationshipsCampaignDecoder : Json.Decode.Decoder PostSpecimenRelationshipsCampaign
postSpecimenRelationshipsCampaignDecoder =
    Json.Decode.succeed PostSpecimenRelationshipsCampaign
        |> Json.Decode.Pipeline.required "data" idAndTypeDecoder
        |> Json.Decode.Pipeline.required "links" postSpecimenRelationshipsCampaignLinksDecoder


postSpecimenRelationshipsCampaignLinksDecoder : Json.Decode.Decoder PostSpecimenRelationshipsCampaignLinks
postSpecimenRelationshipsCampaignLinksDecoder =
    Json.Decode.succeed PostSpecimenRelationshipsCampaignLinks
        |> Json.Decode.Pipeline.required "related" Json.Decode.string


postSpecimenRelationshipsImagesDecoder : Json.Decode.Decoder PostSpecimenRelationshipsImages
postSpecimenRelationshipsImagesDecoder =
    Json.Decode.succeed PostSpecimenRelationshipsImages
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list idAndTypeDecoder)


postSpecimenRelationshipsMediaDecoder : Json.Decode.Decoder PostSpecimenRelationshipsMedia
postSpecimenRelationshipsMediaDecoder =
    Json.Decode.succeed PostSpecimenRelationshipsMedia
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list idAndTypeDecoder)


postSpecimenRelationshipsUserDecoder : Json.Decode.Decoder PostSpecimenRelationshipsUser
postSpecimenRelationshipsUserDecoder =
    Json.Decode.succeed PostSpecimenRelationshipsUser
        |> Json.Decode.Pipeline.required "data" idAndTypeDecoder
        |> Json.Decode.Pipeline.required "links" postSpecimenRelationshipsUserLinksDecoder


postSpecimenRelationshipsUserLinksDecoder : Json.Decode.Decoder PostSpecimenRelationshipsUserLinks
postSpecimenRelationshipsUserLinksDecoder =
    Json.Decode.succeed PostSpecimenRelationshipsUserLinks
        |> Json.Decode.Pipeline.required "related" Json.Decode.string


postSpecimenRelationshipsUserDefinedTagsDecoder : Json.Decode.Decoder PostSpecimenRelationshipsUserDefinedTags
postSpecimenRelationshipsUserDefinedTagsDecoder =
    Json.Decode.succeed PostSpecimenRelationshipsUserDefinedTags
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list idAndTypeDecoder)


postGadgetDecoder : Json.Decode.Decoder PostGadget
postGadgetDecoder =
    Json.Decode.succeed PostGadget
        |> Json.Decode.Pipeline.required "attributes" postGadgetAttributesDecoder
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "relationships" postGadgetRelationshipsDecoder
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postGadgetAttributesDecoder : Json.Decode.Decoder PostGadgetAttributes
postGadgetAttributesDecoder =
    Json.Decode.succeed PostGadgetAttributes
        |> Json.Decode.Pipeline.required "comment_count" Json.Decode.int
        |> Json.Decode.Pipeline.required "commenter_count" Json.Decode.int
        |> Json.Decode.Pipeline.required "content" Json.Decode.string
        |> Json.Decode.Pipeline.required "created_at" Json.Decode.string
        |> Json.Decode.Pipeline.required "current_user_can_comment" Json.Decode.bool
        |> Json.Decode.Pipeline.required "current_user_can_delete" Json.Decode.bool
        |> Json.Decode.Pipeline.required "current_user_can_report" Json.Decode.bool
        |> Json.Decode.Pipeline.required "current_user_can_view" Json.Decode.bool
        |> Json.Decode.Pipeline.required "current_user_has_liked" Json.Decode.bool
        |> Json.Decode.Pipeline.required "has_ti_violation" Json.Decode.bool
        |> Json.Decode.Pipeline.required "image" imageDecoder
        |> Json.Decode.Pipeline.required "is_new_to_current_user" Json.Decode.bool
        |> Json.Decode.Pipeline.required "is_paid" Json.Decode.bool
        |> Json.Decode.Pipeline.required "like_count" Json.Decode.int
        |> Json.Decode.Pipeline.required "meta_image_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "moderation_status" Json.Decode.string
        |> Json.Decode.Pipeline.required "patreon_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "pledge_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "post_file" postFileDecoder
        |> Json.Decode.Pipeline.required "post_metadata" postGadgetAttributesPostMetadataDecoder
        |> Json.Decode.Pipeline.required "post_type" Json.Decode.string
        |> Json.Decode.Pipeline.required "preview_asset_type" Json.Decode.string
        |> Json.Decode.Pipeline.required "published_at" Json.Decode.string
        |> Json.Decode.Pipeline.required "teaser_text" Json.Decode.string
        |> Json.Decode.Pipeline.required "thumbnail" postGadgetAttributesThumbnailDecoder
        |> Json.Decode.Pipeline.required "title" Json.Decode.string
        |> Json.Decode.Pipeline.required "upgrade_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "url" Json.Decode.string
        |> Json.Decode.Pipeline.required "was_posted_by_campaign_owner" Json.Decode.bool


postGadgetAttributesPostMetadataDecoder : Json.Decode.Decoder PostGadgetAttributesPostMetadata
postGadgetAttributesPostMetadataDecoder =
    Json.Decode.succeed PostGadgetAttributesPostMetadata
        |> Json.Decode.Pipeline.required "episode_number" Json.Decode.int
        |> Json.Decode.Pipeline.required "platform" postGadgetAttributesPostMetadataPlatformDecoder
        |> Json.Decode.Pipeline.required "season" Json.Decode.int


postGadgetAttributesPostMetadataPlatformDecoder : Json.Decode.Decoder PostGadgetAttributesPostMetadataPlatform
postGadgetAttributesPostMetadataPlatformDecoder =
    Json.Decode.succeed PostGadgetAttributesPostMetadataPlatform


postGadgetAttributesThumbnailDecoder : Json.Decode.Decoder PostGadgetAttributesThumbnail
postGadgetAttributesThumbnailDecoder =
    Json.Decode.succeed PostGadgetAttributesThumbnail
        |> Json.Decode.Pipeline.required "large" Json.Decode.string
        |> Json.Decode.Pipeline.required "large_2" Json.Decode.string
        |> Json.Decode.Pipeline.required "square" Json.Decode.string
        |> Json.Decode.Pipeline.required "url" Json.Decode.string


postGadgetRelationshipsDecoder : Json.Decode.Decoder PostGadgetRelationships
postGadgetRelationshipsDecoder =
    Json.Decode.succeed PostGadgetRelationships
        |> Json.Decode.Pipeline.required "access_rules" postGadgetRelationshipsAccessRulesDecoder
        |> Json.Decode.Pipeline.required "audio" postGadgetRelationshipsAudioDecoder
        |> Json.Decode.Pipeline.required "campaign" postGadgetRelationshipsCampaignDecoder
        |> Json.Decode.Pipeline.required "media" postGadgetRelationshipsMediaDecoder
        |> Json.Decode.Pipeline.required "user" postGadgetRelationshipsUserDecoder
        |> Json.Decode.Pipeline.required "user_defined_tags" postGadgetRelationshipsUserDefinedTagsDecoder


postGadgetRelationshipsAccessRulesDecoder : Json.Decode.Decoder PostGadgetRelationshipsAccessRules
postGadgetRelationshipsAccessRulesDecoder =
    Json.Decode.succeed PostGadgetRelationshipsAccessRules
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list idAndTypeDecoder)


postGadgetRelationshipsAudioDecoder : Json.Decode.Decoder PostGadgetRelationshipsAudio
postGadgetRelationshipsAudioDecoder =
    Json.Decode.succeed PostGadgetRelationshipsAudio
        |> Json.Decode.Pipeline.required "data" idAndTypeDecoder
        |> Json.Decode.Pipeline.required "links" postGadgetRelationshipsAudioLinksDecoder


postGadgetRelationshipsAudioLinksDecoder : Json.Decode.Decoder PostGadgetRelationshipsAudioLinks
postGadgetRelationshipsAudioLinksDecoder =
    Json.Decode.succeed PostGadgetRelationshipsAudioLinks
        |> Json.Decode.Pipeline.required "related" Json.Decode.string


postGadgetRelationshipsCampaignDecoder : Json.Decode.Decoder PostGadgetRelationshipsCampaign
postGadgetRelationshipsCampaignDecoder =
    Json.Decode.succeed PostGadgetRelationshipsCampaign
        |> Json.Decode.Pipeline.required "data" idAndTypeDecoder
        |> Json.Decode.Pipeline.required "links" postGadgetRelationshipsCampaignLinksDecoder


postGadgetRelationshipsCampaignLinksDecoder : Json.Decode.Decoder PostGadgetRelationshipsCampaignLinks
postGadgetRelationshipsCampaignLinksDecoder =
    Json.Decode.succeed PostGadgetRelationshipsCampaignLinks
        |> Json.Decode.Pipeline.required "related" Json.Decode.string


postGadgetRelationshipsMediaDecoder : Json.Decode.Decoder PostGadgetRelationshipsMedia
postGadgetRelationshipsMediaDecoder =
    Json.Decode.succeed PostGadgetRelationshipsMedia
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list idAndTypeDecoder)


postGadgetRelationshipsUserDecoder : Json.Decode.Decoder PostGadgetRelationshipsUser
postGadgetRelationshipsUserDecoder =
    Json.Decode.succeed PostGadgetRelationshipsUser
        |> Json.Decode.Pipeline.required "data" idAndTypeDecoder
        |> Json.Decode.Pipeline.required "links" postGadgetRelationshipsUserLinksDecoder


postGadgetRelationshipsUserLinksDecoder : Json.Decode.Decoder PostGadgetRelationshipsUserLinks
postGadgetRelationshipsUserLinksDecoder =
    Json.Decode.succeed PostGadgetRelationshipsUserLinks
        |> Json.Decode.Pipeline.required "related" Json.Decode.string


postGadgetRelationshipsUserDefinedTagsDecoder : Json.Decode.Decoder PostGadgetRelationshipsUserDefinedTags
postGadgetRelationshipsUserDefinedTagsDecoder =
    Json.Decode.succeed PostGadgetRelationshipsUserDefinedTags
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list idAndTypeDecoder)


postWidgetDecoder : Json.Decode.Decoder PostWidget
postWidgetDecoder =
    Json.Decode.succeed PostWidget
        |> Json.Decode.Pipeline.required "attributes" postWidgetAttributesDecoder
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "relationships" postWidgetRelationshipsDecoder
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postWidgetAttributesDecoder : Json.Decode.Decoder PostWidgetAttributes
postWidgetAttributesDecoder =
    Json.Decode.succeed PostWidgetAttributes
        |> Json.Decode.Pipeline.required "comment_count" Json.Decode.int
        |> Json.Decode.Pipeline.required "commenter_count" Json.Decode.int
        |> Json.Decode.Pipeline.required "content" Json.Decode.string
        |> Json.Decode.Pipeline.required "created_at" Json.Decode.string
        |> Json.Decode.Pipeline.required "current_user_can_comment" Json.Decode.bool
        |> Json.Decode.Pipeline.required "current_user_can_delete" Json.Decode.bool
        |> Json.Decode.Pipeline.required "current_user_can_report" Json.Decode.bool
        |> Json.Decode.Pipeline.required "current_user_can_view" Json.Decode.bool
        |> Json.Decode.Pipeline.required "current_user_has_liked" Json.Decode.bool
        |> Json.Decode.Pipeline.required "embed" postWidgetAttributesEmbedDecoder
        |> Json.Decode.Pipeline.required "has_ti_violation" Json.Decode.bool
        |> Json.Decode.Pipeline.required "image" imageDecoder
        |> Json.Decode.Pipeline.required "is_new_to_current_user" Json.Decode.bool
        |> Json.Decode.Pipeline.required "is_paid" Json.Decode.bool
        |> Json.Decode.Pipeline.required "like_count" Json.Decode.int
        |> Json.Decode.Pipeline.required "meta_image_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "moderation_status" Json.Decode.string
        |> Json.Decode.Pipeline.required "patreon_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "pledge_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "post_file" postWidgetAttributesPostFileDecoder
        |> Json.Decode.Pipeline.required "post_metadata" postWidgetAttributesPostMetadataDecoder
        |> Json.Decode.Pipeline.required "post_type" Json.Decode.string
        |> Json.Decode.Pipeline.required "preview_asset_type" Json.Decode.string
        |> Json.Decode.Pipeline.required "published_at" Json.Decode.string
        |> Json.Decode.Pipeline.required "teaser_text" Json.Decode.string
        |> Json.Decode.Pipeline.required "thumbnail" postWidgetAttributesThumbnailDecoder
        |> Json.Decode.Pipeline.required "title" Json.Decode.string
        |> Json.Decode.Pipeline.required "upgrade_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "url" Json.Decode.string
        |> Json.Decode.Pipeline.required "was_posted_by_campaign_owner" Json.Decode.bool


postWidgetAttributesEmbedDecoder : Json.Decode.Decoder PostWidgetAttributesEmbed
postWidgetAttributesEmbedDecoder =
    Json.Decode.succeed PostWidgetAttributesEmbed
        |> Json.Decode.Pipeline.required "description" Json.Decode.string
        |> Json.Decode.Pipeline.required "html" Json.Decode.string
        |> Json.Decode.Pipeline.required "provider" Json.Decode.string
        |> Json.Decode.Pipeline.required "provider_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "subject" Json.Decode.string
        |> Json.Decode.Pipeline.required "url" Json.Decode.string


postWidgetAttributesPostFileDecoder : Json.Decode.Decoder PostWidgetAttributesPostFile
postWidgetAttributesPostFileDecoder =
    Json.Decode.succeed PostWidgetAttributesPostFile
        |> Json.Decode.Pipeline.required "height" Json.Decode.int
        |> Json.Decode.Pipeline.required "image_colors" imageColorsDecoder
        |> Json.Decode.Pipeline.required "media_id" Json.Decode.int
        |> Json.Decode.Pipeline.required "state" Json.Decode.string
        |> Json.Decode.Pipeline.required "url" Json.Decode.string
        |> Json.Decode.Pipeline.required "width" Json.Decode.int


postWidgetAttributesPostMetadataDecoder : Json.Decode.Decoder PostWidgetAttributesPostMetadata
postWidgetAttributesPostMetadataDecoder =
    Json.Decode.succeed PostWidgetAttributesPostMetadata
        |> Json.Decode.Pipeline.required "platform" postWidgetAttributesPostMetadataPlatformDecoder


postWidgetAttributesPostMetadataPlatformDecoder : Json.Decode.Decoder PostWidgetAttributesPostMetadataPlatform
postWidgetAttributesPostMetadataPlatformDecoder =
    Json.Decode.succeed PostWidgetAttributesPostMetadataPlatform


postWidgetAttributesThumbnailDecoder : Json.Decode.Decoder PostWidgetAttributesThumbnail
postWidgetAttributesThumbnailDecoder =
    Json.Decode.succeed PostWidgetAttributesThumbnail
        |> Json.Decode.Pipeline.required "large" Json.Decode.string
        |> Json.Decode.Pipeline.required "large_2" Json.Decode.string
        |> Json.Decode.Pipeline.required "square" Json.Decode.string
        |> Json.Decode.Pipeline.required "url" Json.Decode.string


postWidgetRelationshipsDecoder : Json.Decode.Decoder PostWidgetRelationships
postWidgetRelationshipsDecoder =
    Json.Decode.succeed PostWidgetRelationships
        |> Json.Decode.Pipeline.required "access_rules" postWidgetRelationshipsAccessRulesDecoder
        |> Json.Decode.Pipeline.required "campaign" postWidgetRelationshipsCampaignDecoder
        |> Json.Decode.Pipeline.required "images" postWidgetRelationshipsImagesDecoder
        |> Json.Decode.Pipeline.required "media" postWidgetRelationshipsMediaDecoder
        |> Json.Decode.Pipeline.required "user" postWidgetRelationshipsUserDecoder
        |> Json.Decode.Pipeline.required "user_defined_tags" postWidgetRelationshipsUserDefinedTagsDecoder


postWidgetRelationshipsAccessRulesDecoder : Json.Decode.Decoder PostWidgetRelationshipsAccessRules
postWidgetRelationshipsAccessRulesDecoder =
    Json.Decode.succeed PostWidgetRelationshipsAccessRules
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list idAndTypeDecoder)


postWidgetRelationshipsCampaignDecoder : Json.Decode.Decoder PostWidgetRelationshipsCampaign
postWidgetRelationshipsCampaignDecoder =
    Json.Decode.succeed PostWidgetRelationshipsCampaign
        |> Json.Decode.Pipeline.required "data" idAndTypeDecoder
        |> Json.Decode.Pipeline.required "links" postWidgetRelationshipsCampaignLinksDecoder


postWidgetRelationshipsCampaignLinksDecoder : Json.Decode.Decoder PostWidgetRelationshipsCampaignLinks
postWidgetRelationshipsCampaignLinksDecoder =
    Json.Decode.succeed PostWidgetRelationshipsCampaignLinks
        |> Json.Decode.Pipeline.required "related" Json.Decode.string


postWidgetRelationshipsImagesDecoder : Json.Decode.Decoder PostWidgetRelationshipsImages
postWidgetRelationshipsImagesDecoder =
    Json.Decode.succeed PostWidgetRelationshipsImages
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list idAndTypeDecoder)


postWidgetRelationshipsMediaDecoder : Json.Decode.Decoder PostWidgetRelationshipsMedia
postWidgetRelationshipsMediaDecoder =
    Json.Decode.succeed PostWidgetRelationshipsMedia
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list idAndTypeDecoder)


postWidgetRelationshipsUserDecoder : Json.Decode.Decoder PostWidgetRelationshipsUser
postWidgetRelationshipsUserDecoder =
    Json.Decode.succeed PostWidgetRelationshipsUser
        |> Json.Decode.Pipeline.required "data" idAndTypeDecoder
        |> Json.Decode.Pipeline.required "links" postWidgetRelationshipsUserLinksDecoder


postWidgetRelationshipsUserLinksDecoder : Json.Decode.Decoder PostWidgetRelationshipsUserLinks
postWidgetRelationshipsUserLinksDecoder =
    Json.Decode.succeed PostWidgetRelationshipsUserLinks
        |> Json.Decode.Pipeline.required "related" Json.Decode.string


postWidgetRelationshipsUserDefinedTagsDecoder : Json.Decode.Decoder PostWidgetRelationshipsUserDefinedTags
postWidgetRelationshipsUserDefinedTagsDecoder =
    Json.Decode.succeed PostWidgetRelationshipsUserDefinedTags
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list idAndTypeDecoder)


postGizmoDecoder : Json.Decode.Decoder PostGizmo
postGizmoDecoder =
    Json.Decode.succeed PostGizmo
        |> Json.Decode.Pipeline.required "attributes" postGizmoAttributesDecoder
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "relationships" postGizmoRelationshipsDecoder
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postGizmoAttributesDecoder : Json.Decode.Decoder PostGizmoAttributes
postGizmoAttributesDecoder =
    Json.Decode.succeed PostGizmoAttributes
        |> Json.Decode.Pipeline.required "comment_count" Json.Decode.int
        |> Json.Decode.Pipeline.required "commenter_count" Json.Decode.int
        |> Json.Decode.Pipeline.required "content" Json.Decode.string
        |> Json.Decode.Pipeline.required "created_at" Json.Decode.string
        |> Json.Decode.Pipeline.required "current_user_can_comment" Json.Decode.bool
        |> Json.Decode.Pipeline.required "current_user_can_delete" Json.Decode.bool
        |> Json.Decode.Pipeline.required "current_user_can_report" Json.Decode.bool
        |> Json.Decode.Pipeline.required "current_user_can_view" Json.Decode.bool
        |> Json.Decode.Pipeline.required "current_user_has_liked" Json.Decode.bool
        |> Json.Decode.Pipeline.required "has_ti_violation" Json.Decode.bool
        |> Json.Decode.Pipeline.required "image" imageDecoder
        |> Json.Decode.Pipeline.required "is_new_to_current_user" Json.Decode.bool
        |> Json.Decode.Pipeline.required "is_paid" Json.Decode.bool
        |> Json.Decode.Pipeline.required "like_count" Json.Decode.int
        |> Json.Decode.Pipeline.required "meta_image_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "moderation_status" Json.Decode.string
        |> Json.Decode.Pipeline.required "patreon_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "pledge_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "post_file" postFileDecoder
        |> Json.Decode.Pipeline.required "post_metadata" postGizmoAttributesPostMetadataDecoder
        |> Json.Decode.Pipeline.required "post_type" Json.Decode.string
        |> Json.Decode.Pipeline.required "preview_asset_type" Json.Decode.string
        |> Json.Decode.Pipeline.required "published_at" Json.Decode.string
        |> Json.Decode.Pipeline.required "teaser_text" Json.Decode.string
        |> Json.Decode.Pipeline.required "thumbnail" postGizmoAttributesThumbnailDecoder
        |> Json.Decode.Pipeline.required "title" Json.Decode.string
        |> Json.Decode.Pipeline.required "upgrade_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "url" Json.Decode.string
        |> Json.Decode.Pipeline.required "was_posted_by_campaign_owner" Json.Decode.bool


postGizmoAttributesPostMetadataDecoder : Json.Decode.Decoder PostGizmoAttributesPostMetadata
postGizmoAttributesPostMetadataDecoder =
    Json.Decode.succeed PostGizmoAttributesPostMetadata
        |> Json.Decode.Pipeline.required "image_order" (Json.Decode.list Json.Decode.string)
        |> Json.Decode.Pipeline.required "platform" postGizmoAttributesPostMetadataPlatformDecoder


postGizmoAttributesPostMetadataPlatformDecoder : Json.Decode.Decoder PostGizmoAttributesPostMetadataPlatform
postGizmoAttributesPostMetadataPlatformDecoder =
    Json.Decode.succeed PostGizmoAttributesPostMetadataPlatform


postGizmoAttributesThumbnailDecoder : Json.Decode.Decoder PostGizmoAttributesThumbnail
postGizmoAttributesThumbnailDecoder =
    Json.Decode.succeed PostGizmoAttributesThumbnail
        |> Json.Decode.Pipeline.required "large" Json.Decode.string
        |> Json.Decode.Pipeline.required "large_2" Json.Decode.string
        |> Json.Decode.Pipeline.required "square" Json.Decode.string
        |> Json.Decode.Pipeline.required "url" Json.Decode.string


postGizmoRelationshipsDecoder : Json.Decode.Decoder PostGizmoRelationships
postGizmoRelationshipsDecoder =
    Json.Decode.succeed PostGizmoRelationships
        |> Json.Decode.Pipeline.required "access_rules" postGizmoRelationshipsAccessRulesDecoder
        |> Json.Decode.Pipeline.required "audio" postGizmoRelationshipsAudioDecoder
        |> Json.Decode.Pipeline.required "audio_preview" postGizmoRelationshipsAudioPreviewDecoder
        |> Json.Decode.Pipeline.required "campaign" postGizmoRelationshipsCampaignDecoder
        |> Json.Decode.Pipeline.required "images" postGizmoRelationshipsImagesDecoder
        |> Json.Decode.Pipeline.required "media" postGizmoRelationshipsMediaDecoder
        |> Json.Decode.Pipeline.required "user" postGizmoRelationshipsUserDecoder


postGizmoRelationshipsAccessRulesDecoder : Json.Decode.Decoder PostGizmoRelationshipsAccessRules
postGizmoRelationshipsAccessRulesDecoder =
    Json.Decode.succeed PostGizmoRelationshipsAccessRules
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list idAndTypeDecoder)


postGizmoRelationshipsAudioDecoder : Json.Decode.Decoder PostGizmoRelationshipsAudio
postGizmoRelationshipsAudioDecoder =
    Json.Decode.succeed PostGizmoRelationshipsAudio
        |> Json.Decode.Pipeline.required "data" idAndTypeDecoder
        |> Json.Decode.Pipeline.required "links" postGizmoRelationshipsAudioLinksDecoder


postGizmoRelationshipsAudioLinksDecoder : Json.Decode.Decoder PostGizmoRelationshipsAudioLinks
postGizmoRelationshipsAudioLinksDecoder =
    Json.Decode.succeed PostGizmoRelationshipsAudioLinks
        |> Json.Decode.Pipeline.required "related" Json.Decode.string


postGizmoRelationshipsAudioPreviewDecoder : Json.Decode.Decoder PostGizmoRelationshipsAudioPreview
postGizmoRelationshipsAudioPreviewDecoder =
    Json.Decode.succeed PostGizmoRelationshipsAudioPreview
        |> Json.Decode.Pipeline.required "data" idAndTypeDecoder
        |> Json.Decode.Pipeline.required "links" postGizmoRelationshipsAudioPreviewLinksDecoder


postGizmoRelationshipsAudioPreviewLinksDecoder : Json.Decode.Decoder PostGizmoRelationshipsAudioPreviewLinks
postGizmoRelationshipsAudioPreviewLinksDecoder =
    Json.Decode.succeed PostGizmoRelationshipsAudioPreviewLinks
        |> Json.Decode.Pipeline.required "related" Json.Decode.string


postGizmoRelationshipsCampaignDecoder : Json.Decode.Decoder PostGizmoRelationshipsCampaign
postGizmoRelationshipsCampaignDecoder =
    Json.Decode.succeed PostGizmoRelationshipsCampaign
        |> Json.Decode.Pipeline.required "data" idAndTypeDecoder
        |> Json.Decode.Pipeline.required "links" postGizmoRelationshipsCampaignLinksDecoder


postGizmoRelationshipsCampaignLinksDecoder : Json.Decode.Decoder PostGizmoRelationshipsCampaignLinks
postGizmoRelationshipsCampaignLinksDecoder =
    Json.Decode.succeed PostGizmoRelationshipsCampaignLinks
        |> Json.Decode.Pipeline.required "related" Json.Decode.string


postGizmoRelationshipsImagesDecoder : Json.Decode.Decoder PostGizmoRelationshipsImages
postGizmoRelationshipsImagesDecoder =
    Json.Decode.succeed PostGizmoRelationshipsImages
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list idAndTypeDecoder)


postGizmoRelationshipsMediaDecoder : Json.Decode.Decoder PostGizmoRelationshipsMedia
postGizmoRelationshipsMediaDecoder =
    Json.Decode.succeed PostGizmoRelationshipsMedia
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list idAndTypeDecoder)


postGizmoRelationshipsUserDecoder : Json.Decode.Decoder PostGizmoRelationshipsUser
postGizmoRelationshipsUserDecoder =
    Json.Decode.succeed PostGizmoRelationshipsUser
        |> Json.Decode.Pipeline.required "data" idAndTypeDecoder
        |> Json.Decode.Pipeline.required "links" postGizmoRelationshipsUserLinksDecoder


postGizmoRelationshipsUserLinksDecoder : Json.Decode.Decoder PostGizmoRelationshipsUserLinks
postGizmoRelationshipsUserLinksDecoder =
    Json.Decode.succeed PostGizmoRelationshipsUserLinks
        |> Json.Decode.Pipeline.required "related" Json.Decode.string


postPartDecoder : Json.Decode.Decoder PostPart
postPartDecoder =
    Json.Decode.succeed PostPart
        |> Json.Decode.Pipeline.required "attributes" postPartAttributesDecoder
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "relationships" postPartRelationshipsDecoder
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postPartAttributesDecoder : Json.Decode.Decoder PostPartAttributes
postPartAttributesDecoder =
    Json.Decode.succeed PostPartAttributes
        |> Json.Decode.Pipeline.required "comment_count" Json.Decode.int
        |> Json.Decode.Pipeline.required "commenter_count" Json.Decode.int
        |> Json.Decode.Pipeline.required "content" Json.Decode.string
        |> Json.Decode.Pipeline.required "created_at" Json.Decode.string
        |> Json.Decode.Pipeline.required "current_user_can_comment" Json.Decode.bool
        |> Json.Decode.Pipeline.required "current_user_can_delete" Json.Decode.bool
        |> Json.Decode.Pipeline.required "current_user_can_report" Json.Decode.bool
        |> Json.Decode.Pipeline.required "current_user_can_view" Json.Decode.bool
        |> Json.Decode.Pipeline.required "current_user_has_liked" Json.Decode.bool
        |> Json.Decode.Pipeline.required "has_ti_violation" Json.Decode.bool
        |> Json.Decode.Pipeline.required "image" imageDecoder
        |> Json.Decode.Pipeline.required "is_new_to_current_user" Json.Decode.bool
        |> Json.Decode.Pipeline.required "is_paid" Json.Decode.bool
        |> Json.Decode.Pipeline.required "like_count" Json.Decode.int
        |> Json.Decode.Pipeline.required "meta_image_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "moderation_status" Json.Decode.string
        |> Json.Decode.Pipeline.required "patreon_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "pledge_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "post_file" postFileDecoder
        |> Json.Decode.Pipeline.required "post_metadata" postPartAttributesPostMetadataDecoder
        |> Json.Decode.Pipeline.required "post_type" Json.Decode.string
        |> Json.Decode.Pipeline.required "preview_asset_type" Json.Decode.string
        |> Json.Decode.Pipeline.required "published_at" Json.Decode.string
        |> Json.Decode.Pipeline.required "teaser_text" Json.Decode.string
        |> Json.Decode.Pipeline.required "thumbnail" postPartAttributesThumbnailDecoder
        |> Json.Decode.Pipeline.required "title" Json.Decode.string
        |> Json.Decode.Pipeline.required "upgrade_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "url" Json.Decode.string
        |> Json.Decode.Pipeline.required "was_posted_by_campaign_owner" Json.Decode.bool


postPartAttributesPostMetadataDecoder : Json.Decode.Decoder PostPartAttributesPostMetadata
postPartAttributesPostMetadataDecoder =
    Json.Decode.succeed PostPartAttributesPostMetadata
        |> Json.Decode.Pipeline.required "platform" postPartAttributesPostMetadataPlatformDecoder


postPartAttributesPostMetadataPlatformDecoder : Json.Decode.Decoder PostPartAttributesPostMetadataPlatform
postPartAttributesPostMetadataPlatformDecoder =
    Json.Decode.succeed PostPartAttributesPostMetadataPlatform


postPartAttributesThumbnailDecoder : Json.Decode.Decoder PostPartAttributesThumbnail
postPartAttributesThumbnailDecoder =
    Json.Decode.succeed PostPartAttributesThumbnail
        |> Json.Decode.Pipeline.required "large" Json.Decode.string
        |> Json.Decode.Pipeline.required "large_2" Json.Decode.string
        |> Json.Decode.Pipeline.required "square" Json.Decode.string
        |> Json.Decode.Pipeline.required "url" Json.Decode.string


postPartRelationshipsDecoder : Json.Decode.Decoder PostPartRelationships
postPartRelationshipsDecoder =
    Json.Decode.succeed PostPartRelationships
        |> Json.Decode.Pipeline.required "access_rules" postPartRelationshipsAccessRulesDecoder
        |> Json.Decode.Pipeline.required "attachments_media" postPartRelationshipsAttachmentsMediaDecoder
        |> Json.Decode.Pipeline.required "audio" postPartRelationshipsAudioDecoder
        |> Json.Decode.Pipeline.required "audio_preview" postPartRelationshipsAudioPreviewDecoder
        |> Json.Decode.Pipeline.required "campaign" postPartRelationshipsCampaignDecoder
        |> Json.Decode.Pipeline.required "media" postPartRelationshipsMediaDecoder
        |> Json.Decode.Pipeline.required "user" postPartRelationshipsUserDecoder
        |> Json.Decode.Pipeline.required "user_defined_tags" postPartRelationshipsUserDefinedTagsDecoder


postPartRelationshipsAccessRulesDecoder : Json.Decode.Decoder PostPartRelationshipsAccessRules
postPartRelationshipsAccessRulesDecoder =
    Json.Decode.succeed PostPartRelationshipsAccessRules
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list idAndTypeDecoder)


postPartRelationshipsAttachmentsMediaDecoder : Json.Decode.Decoder PostPartRelationshipsAttachmentsMedia
postPartRelationshipsAttachmentsMediaDecoder =
    Json.Decode.succeed PostPartRelationshipsAttachmentsMedia
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list idAndTypeDecoder)


postPartRelationshipsAudioDecoder : Json.Decode.Decoder PostPartRelationshipsAudio
postPartRelationshipsAudioDecoder =
    Json.Decode.succeed PostPartRelationshipsAudio
        |> Json.Decode.Pipeline.required "data" idAndTypeDecoder
        |> Json.Decode.Pipeline.required "links" postPartRelationshipsAudioLinksDecoder


postPartRelationshipsAudioLinksDecoder : Json.Decode.Decoder PostPartRelationshipsAudioLinks
postPartRelationshipsAudioLinksDecoder =
    Json.Decode.succeed PostPartRelationshipsAudioLinks
        |> Json.Decode.Pipeline.required "related" Json.Decode.string


postPartRelationshipsAudioPreviewDecoder : Json.Decode.Decoder PostPartRelationshipsAudioPreview
postPartRelationshipsAudioPreviewDecoder =
    Json.Decode.succeed PostPartRelationshipsAudioPreview
        |> Json.Decode.Pipeline.required "data" idAndTypeDecoder
        |> Json.Decode.Pipeline.required "links" postPartRelationshipsAudioPreviewLinksDecoder


postPartRelationshipsAudioPreviewLinksDecoder : Json.Decode.Decoder PostPartRelationshipsAudioPreviewLinks
postPartRelationshipsAudioPreviewLinksDecoder =
    Json.Decode.succeed PostPartRelationshipsAudioPreviewLinks
        |> Json.Decode.Pipeline.required "related" Json.Decode.string


postPartRelationshipsCampaignDecoder : Json.Decode.Decoder PostPartRelationshipsCampaign
postPartRelationshipsCampaignDecoder =
    Json.Decode.succeed PostPartRelationshipsCampaign
        |> Json.Decode.Pipeline.required "data" idAndTypeDecoder
        |> Json.Decode.Pipeline.required "links" postPartRelationshipsCampaignLinksDecoder


postPartRelationshipsCampaignLinksDecoder : Json.Decode.Decoder PostPartRelationshipsCampaignLinks
postPartRelationshipsCampaignLinksDecoder =
    Json.Decode.succeed PostPartRelationshipsCampaignLinks
        |> Json.Decode.Pipeline.required "related" Json.Decode.string


postPartRelationshipsMediaDecoder : Json.Decode.Decoder PostPartRelationshipsMedia
postPartRelationshipsMediaDecoder =
    Json.Decode.succeed PostPartRelationshipsMedia
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list idAndTypeDecoder)


postPartRelationshipsUserDecoder : Json.Decode.Decoder PostPartRelationshipsUser
postPartRelationshipsUserDecoder =
    Json.Decode.succeed PostPartRelationshipsUser
        |> Json.Decode.Pipeline.required "data" idAndTypeDecoder
        |> Json.Decode.Pipeline.required "links" postPartRelationshipsUserLinksDecoder


postPartRelationshipsUserLinksDecoder : Json.Decode.Decoder PostPartRelationshipsUserLinks
postPartRelationshipsUserLinksDecoder =
    Json.Decode.succeed PostPartRelationshipsUserLinks
        |> Json.Decode.Pipeline.required "related" Json.Decode.string


postPartRelationshipsUserDefinedTagsDecoder : Json.Decode.Decoder PostPartRelationshipsUserDefinedTags
postPartRelationshipsUserDefinedTagsDecoder =
    Json.Decode.succeed PostPartRelationshipsUserDefinedTags
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list idAndTypeDecoder)


postChunkDecoder : Json.Decode.Decoder PostChunk
postChunkDecoder =
    Json.Decode.succeed PostChunk
        |> Json.Decode.Pipeline.required "attributes" postChunkAttributesDecoder
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "relationships" postChunkRelationshipsDecoder
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postChunkAttributesDecoder : Json.Decode.Decoder PostChunkAttributes
postChunkAttributesDecoder =
    Json.Decode.succeed PostChunkAttributes
        |> Json.Decode.Pipeline.required "comment_count" Json.Decode.int
        |> Json.Decode.Pipeline.required "commenter_count" Json.Decode.int
        |> Json.Decode.Pipeline.required "content" Json.Decode.string
        |> Json.Decode.Pipeline.required "created_at" Json.Decode.string
        |> Json.Decode.Pipeline.required "current_user_can_comment" Json.Decode.bool
        |> Json.Decode.Pipeline.required "current_user_can_delete" Json.Decode.bool
        |> Json.Decode.Pipeline.required "current_user_can_report" Json.Decode.bool
        |> Json.Decode.Pipeline.required "current_user_can_view" Json.Decode.bool
        |> Json.Decode.Pipeline.required "current_user_has_liked" Json.Decode.bool
        |> Json.Decode.Pipeline.required "embed" postChunkAttributesEmbedDecoder
        |> Json.Decode.Pipeline.required "has_ti_violation" Json.Decode.bool
        |> Json.Decode.Pipeline.required "image" imageDecoder
        |> Json.Decode.Pipeline.required "is_new_to_current_user" Json.Decode.bool
        |> Json.Decode.Pipeline.required "is_paid" Json.Decode.bool
        |> Json.Decode.Pipeline.required "like_count" Json.Decode.int
        |> Json.Decode.Pipeline.required "meta_image_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "min_cents_pledged_to_view" Json.Decode.int
        |> Json.Decode.Pipeline.required "moderation_status" Json.Decode.string
        |> Json.Decode.Pipeline.required "patreon_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "pledge_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "post_file" postChunkAttributesPostFileDecoder
        |> Json.Decode.Pipeline.required "post_metadata" postChunkAttributesPostMetadataDecoder
        |> Json.Decode.Pipeline.required "post_type" Json.Decode.string
        |> Json.Decode.Pipeline.required "preview_asset_type" Json.Decode.string
        |> Json.Decode.Pipeline.required "published_at" Json.Decode.string
        |> Json.Decode.Pipeline.required "teaser_text" Json.Decode.string
        |> Json.Decode.Pipeline.required "thumbnail" postChunkAttributesThumbnailDecoder
        |> Json.Decode.Pipeline.required "title" Json.Decode.string
        |> Json.Decode.Pipeline.required "upgrade_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "url" Json.Decode.string
        |> Json.Decode.Pipeline.required "was_posted_by_campaign_owner" Json.Decode.bool


postChunkAttributesEmbedDecoder : Json.Decode.Decoder PostChunkAttributesEmbed
postChunkAttributesEmbedDecoder =
    Json.Decode.succeed PostChunkAttributesEmbed
        |> Json.Decode.Pipeline.required "description" Json.Decode.string
        |> Json.Decode.Pipeline.required "html" Json.Decode.string
        |> Json.Decode.Pipeline.required "provider" Json.Decode.string
        |> Json.Decode.Pipeline.required "provider_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "subject" Json.Decode.string
        |> Json.Decode.Pipeline.required "url" Json.Decode.string


postChunkAttributesPostFileDecoder : Json.Decode.Decoder PostChunkAttributesPostFile
postChunkAttributesPostFileDecoder =
    Json.Decode.succeed PostChunkAttributesPostFile
        |> Json.Decode.Pipeline.required "height" Json.Decode.int
        |> Json.Decode.Pipeline.required "image_colors" imageColorsDecoder
        |> Json.Decode.Pipeline.required "media_id" Json.Decode.int
        |> Json.Decode.Pipeline.required "state" Json.Decode.string
        |> Json.Decode.Pipeline.required "url" Json.Decode.string
        |> Json.Decode.Pipeline.required "width" Json.Decode.int


postChunkAttributesPostMetadataDecoder : Json.Decode.Decoder PostChunkAttributesPostMetadata
postChunkAttributesPostMetadataDecoder =
    Json.Decode.succeed PostChunkAttributesPostMetadata
        |> Json.Decode.Pipeline.required "platform" postChunkAttributesPostMetadataPlatformDecoder


postChunkAttributesPostMetadataPlatformDecoder : Json.Decode.Decoder PostChunkAttributesPostMetadataPlatform
postChunkAttributesPostMetadataPlatformDecoder =
    Json.Decode.succeed PostChunkAttributesPostMetadataPlatform


postChunkAttributesThumbnailDecoder : Json.Decode.Decoder PostChunkAttributesThumbnail
postChunkAttributesThumbnailDecoder =
    Json.Decode.succeed PostChunkAttributesThumbnail
        |> Json.Decode.Pipeline.required "large" Json.Decode.string
        |> Json.Decode.Pipeline.required "large_2" Json.Decode.string
        |> Json.Decode.Pipeline.required "square" Json.Decode.string
        |> Json.Decode.Pipeline.required "url" Json.Decode.string


postChunkRelationshipsDecoder : Json.Decode.Decoder PostChunkRelationships
postChunkRelationshipsDecoder =
    Json.Decode.succeed PostChunkRelationships
        |> Json.Decode.Pipeline.required "access_rules" postChunkRelationshipsAccessRulesDecoder
        |> Json.Decode.Pipeline.required "campaign" postChunkRelationshipsCampaignDecoder
        |> Json.Decode.Pipeline.required "images" postChunkRelationshipsImagesDecoder
        |> Json.Decode.Pipeline.required "media" postChunkRelationshipsMediaDecoder
        |> Json.Decode.Pipeline.required "user" postChunkRelationshipsUserDecoder


postChunkRelationshipsAccessRulesDecoder : Json.Decode.Decoder PostChunkRelationshipsAccessRules
postChunkRelationshipsAccessRulesDecoder =
    Json.Decode.succeed PostChunkRelationshipsAccessRules
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list idAndTypeDecoder)


postChunkRelationshipsCampaignDecoder : Json.Decode.Decoder PostChunkRelationshipsCampaign
postChunkRelationshipsCampaignDecoder =
    Json.Decode.succeed PostChunkRelationshipsCampaign
        |> Json.Decode.Pipeline.required "data" idAndTypeDecoder
        |> Json.Decode.Pipeline.required "links" postChunkRelationshipsCampaignLinksDecoder


postChunkRelationshipsCampaignLinksDecoder : Json.Decode.Decoder PostChunkRelationshipsCampaignLinks
postChunkRelationshipsCampaignLinksDecoder =
    Json.Decode.succeed PostChunkRelationshipsCampaignLinks
        |> Json.Decode.Pipeline.required "related" Json.Decode.string


postChunkRelationshipsImagesDecoder : Json.Decode.Decoder PostChunkRelationshipsImages
postChunkRelationshipsImagesDecoder =
    Json.Decode.succeed PostChunkRelationshipsImages
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list idAndTypeDecoder)


postChunkRelationshipsMediaDecoder : Json.Decode.Decoder PostChunkRelationshipsMedia
postChunkRelationshipsMediaDecoder =
    Json.Decode.succeed PostChunkRelationshipsMedia
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list idAndTypeDecoder)


postChunkRelationshipsUserDecoder : Json.Decode.Decoder PostChunkRelationshipsUser
postChunkRelationshipsUserDecoder =
    Json.Decode.succeed PostChunkRelationshipsUser
        |> Json.Decode.Pipeline.required "data" idAndTypeDecoder
        |> Json.Decode.Pipeline.required "links" postChunkRelationshipsUserLinksDecoder


postChunkRelationshipsUserLinksDecoder : Json.Decode.Decoder PostChunkRelationshipsUserLinks
postChunkRelationshipsUserLinksDecoder =
    Json.Decode.succeed PostChunkRelationshipsUserLinks
        |> Json.Decode.Pipeline.required "related" Json.Decode.string


postPieceDecoder : Json.Decode.Decoder PostPiece
postPieceDecoder =
    Json.Decode.succeed PostPiece
        |> Json.Decode.Pipeline.required "attributes" postPieceAttributesDecoder
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "relationships" postPieceRelationshipsDecoder
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postPieceAttributesDecoder : Json.Decode.Decoder PostPieceAttributes
postPieceAttributesDecoder =
    Json.Decode.succeed PostPieceAttributes
        |> Json.Decode.Pipeline.required "comment_count" Json.Decode.int
        |> Json.Decode.Pipeline.required "commenter_count" Json.Decode.int
        |> Json.Decode.Pipeline.required "content" Json.Decode.string
        |> Json.Decode.Pipeline.required "created_at" Json.Decode.string
        |> Json.Decode.Pipeline.required "current_user_can_comment" Json.Decode.bool
        |> Json.Decode.Pipeline.required "current_user_can_delete" Json.Decode.bool
        |> Json.Decode.Pipeline.required "current_user_can_report" Json.Decode.bool
        |> Json.Decode.Pipeline.required "current_user_can_view" Json.Decode.bool
        |> Json.Decode.Pipeline.required "current_user_has_liked" Json.Decode.bool
        |> Json.Decode.Pipeline.required "embed" postPieceAttributesEmbedDecoder
        |> Json.Decode.Pipeline.required "has_ti_violation" Json.Decode.bool
        |> Json.Decode.Pipeline.required "image" imageDecoder
        |> Json.Decode.Pipeline.required "is_new_to_current_user" Json.Decode.bool
        |> Json.Decode.Pipeline.required "is_paid" Json.Decode.bool
        |> Json.Decode.Pipeline.required "like_count" Json.Decode.int
        |> Json.Decode.Pipeline.required "meta_image_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "moderation_status" Json.Decode.string
        |> Json.Decode.Pipeline.required "patreon_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "pledge_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "post_file" postPieceAttributesPostFileDecoder
        |> Json.Decode.Pipeline.required "post_metadata" postPieceAttributesPostMetadataDecoder
        |> Json.Decode.Pipeline.required "post_type" Json.Decode.string
        |> Json.Decode.Pipeline.required "preview_asset_type" Json.Decode.string
        |> Json.Decode.Pipeline.required "published_at" Json.Decode.string
        |> Json.Decode.Pipeline.required "teaser_text" Json.Decode.string
        |> Json.Decode.Pipeline.required "thumbnail" postPieceAttributesThumbnailDecoder
        |> Json.Decode.Pipeline.required "title" Json.Decode.string
        |> Json.Decode.Pipeline.required "upgrade_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "url" Json.Decode.string
        |> Json.Decode.Pipeline.required "was_posted_by_campaign_owner" Json.Decode.bool


postPieceAttributesEmbedDecoder : Json.Decode.Decoder PostPieceAttributesEmbed
postPieceAttributesEmbedDecoder =
    Json.Decode.succeed PostPieceAttributesEmbed
        |> Json.Decode.Pipeline.required "description" Json.Decode.string
        |> Json.Decode.Pipeline.required "html" Json.Decode.string
        |> Json.Decode.Pipeline.required "provider" Json.Decode.string
        |> Json.Decode.Pipeline.required "provider_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "subject" Json.Decode.string
        |> Json.Decode.Pipeline.required "url" Json.Decode.string


postPieceAttributesPostFileDecoder : Json.Decode.Decoder PostPieceAttributesPostFile
postPieceAttributesPostFileDecoder =
    Json.Decode.succeed PostPieceAttributesPostFile
        |> Json.Decode.Pipeline.required "height" Json.Decode.int
        |> Json.Decode.Pipeline.required "image_colors" imageColorsDecoder
        |> Json.Decode.Pipeline.required "media_id" Json.Decode.int
        |> Json.Decode.Pipeline.required "state" Json.Decode.string
        |> Json.Decode.Pipeline.required "url" Json.Decode.string
        |> Json.Decode.Pipeline.required "width" Json.Decode.int


postPieceAttributesPostMetadataDecoder : Json.Decode.Decoder PostPieceAttributesPostMetadata
postPieceAttributesPostMetadataDecoder =
    Json.Decode.succeed PostPieceAttributesPostMetadata
        |> Json.Decode.Pipeline.required "platform" postPieceAttributesPostMetadataPlatformDecoder


postPieceAttributesPostMetadataPlatformDecoder : Json.Decode.Decoder PostPieceAttributesPostMetadataPlatform
postPieceAttributesPostMetadataPlatformDecoder =
    Json.Decode.succeed PostPieceAttributesPostMetadataPlatform


postPieceAttributesThumbnailDecoder : Json.Decode.Decoder PostPieceAttributesThumbnail
postPieceAttributesThumbnailDecoder =
    Json.Decode.succeed PostPieceAttributesThumbnail
        |> Json.Decode.Pipeline.required "large" Json.Decode.string
        |> Json.Decode.Pipeline.required "large_2" Json.Decode.string
        |> Json.Decode.Pipeline.required "square" Json.Decode.string
        |> Json.Decode.Pipeline.required "url" Json.Decode.string


postPieceRelationshipsDecoder : Json.Decode.Decoder PostPieceRelationships
postPieceRelationshipsDecoder =
    Json.Decode.succeed PostPieceRelationships
        |> Json.Decode.Pipeline.required "access_rules" postPieceRelationshipsAccessRulesDecoder
        |> Json.Decode.Pipeline.required "campaign" postPieceRelationshipsCampaignDecoder
        |> Json.Decode.Pipeline.required "images" postPieceRelationshipsImagesDecoder
        |> Json.Decode.Pipeline.required "media" postPieceRelationshipsMediaDecoder
        |> Json.Decode.Pipeline.required "user" postPieceRelationshipsUserDecoder


postPieceRelationshipsAccessRulesDecoder : Json.Decode.Decoder PostPieceRelationshipsAccessRules
postPieceRelationshipsAccessRulesDecoder =
    Json.Decode.succeed PostPieceRelationshipsAccessRules
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list idAndTypeDecoder)


postPieceRelationshipsCampaignDecoder : Json.Decode.Decoder PostPieceRelationshipsCampaign
postPieceRelationshipsCampaignDecoder =
    Json.Decode.succeed PostPieceRelationshipsCampaign
        |> Json.Decode.Pipeline.required "data" idAndTypeDecoder
        |> Json.Decode.Pipeline.required "links" postPieceRelationshipsCampaignLinksDecoder


postPieceRelationshipsCampaignLinksDecoder : Json.Decode.Decoder PostPieceRelationshipsCampaignLinks
postPieceRelationshipsCampaignLinksDecoder =
    Json.Decode.succeed PostPieceRelationshipsCampaignLinks
        |> Json.Decode.Pipeline.required "related" Json.Decode.string


postPieceRelationshipsImagesDecoder : Json.Decode.Decoder PostPieceRelationshipsImages
postPieceRelationshipsImagesDecoder =
    Json.Decode.succeed PostPieceRelationshipsImages
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list idAndTypeDecoder)


postPieceRelationshipsMediaDecoder : Json.Decode.Decoder PostPieceRelationshipsMedia
postPieceRelationshipsMediaDecoder =
    Json.Decode.succeed PostPieceRelationshipsMedia
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list idAndTypeDecoder)


postPieceRelationshipsUserDecoder : Json.Decode.Decoder PostPieceRelationshipsUser
postPieceRelationshipsUserDecoder =
    Json.Decode.succeed PostPieceRelationshipsUser
        |> Json.Decode.Pipeline.required "data" idAndTypeDecoder
        |> Json.Decode.Pipeline.required "links" postPieceRelationshipsUserLinksDecoder


postPieceRelationshipsUserLinksDecoder : Json.Decode.Decoder PostPieceRelationshipsUserLinks
postPieceRelationshipsUserLinksDecoder =
    Json.Decode.succeed PostPieceRelationshipsUserLinks
        |> Json.Decode.Pipeline.required "related" Json.Decode.string


postThingyDecoder : Json.Decode.Decoder PostThingy
postThingyDecoder =
    Json.Decode.succeed PostThingy
        |> Json.Decode.Pipeline.required "attributes" postThingyAttributesDecoder
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "relationships" postThingyRelationshipsDecoder
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postThingyAttributesDecoder : Json.Decode.Decoder PostThingyAttributes
postThingyAttributesDecoder =
    Json.Decode.succeed PostThingyAttributes
        |> Json.Decode.Pipeline.required "comment_count" Json.Decode.int
        |> Json.Decode.Pipeline.required "commenter_count" Json.Decode.int
        |> Json.Decode.Pipeline.required "content" Json.Decode.string
        |> Json.Decode.Pipeline.required "created_at" Json.Decode.string
        |> Json.Decode.Pipeline.required "current_user_can_comment" Json.Decode.bool
        |> Json.Decode.Pipeline.required "current_user_can_delete" Json.Decode.bool
        |> Json.Decode.Pipeline.required "current_user_can_report" Json.Decode.bool
        |> Json.Decode.Pipeline.required "current_user_can_view" Json.Decode.bool
        |> Json.Decode.Pipeline.required "current_user_has_liked" Json.Decode.bool
        |> Json.Decode.Pipeline.required "has_ti_violation" Json.Decode.bool
        |> Json.Decode.Pipeline.required "image" imageDecoder
        |> Json.Decode.Pipeline.required "is_new_to_current_user" Json.Decode.bool
        |> Json.Decode.Pipeline.required "is_paid" Json.Decode.bool
        |> Json.Decode.Pipeline.required "like_count" Json.Decode.int
        |> Json.Decode.Pipeline.required "meta_image_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "moderation_status" Json.Decode.string
        |> Json.Decode.Pipeline.required "patreon_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "pledge_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "post_file" postThingyAttributesPostFileDecoder
        |> Json.Decode.Pipeline.required "post_metadata" postThingyAttributesPostMetadataDecoder
        |> Json.Decode.Pipeline.required "post_type" Json.Decode.string
        |> Json.Decode.Pipeline.required "preview_asset_type" Json.Decode.string
        |> Json.Decode.Pipeline.required "published_at" Json.Decode.string
        |> Json.Decode.Pipeline.required "teaser_text" Json.Decode.string
        |> Json.Decode.Pipeline.required "thumbnail" postThingyAttributesThumbnailDecoder
        |> Json.Decode.Pipeline.required "title" Json.Decode.string
        |> Json.Decode.Pipeline.required "upgrade_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "url" Json.Decode.string
        |> Json.Decode.Pipeline.required "was_posted_by_campaign_owner" Json.Decode.bool


postThingyAttributesPostFileDecoder : Json.Decode.Decoder PostThingyAttributesPostFile
postThingyAttributesPostFileDecoder =
    Json.Decode.succeed PostThingyAttributesPostFile
        |> Json.Decode.Pipeline.required "height" Json.Decode.int
        |> Json.Decode.Pipeline.required "image_colors" imageColorsDecoder
        |> Json.Decode.Pipeline.required "media_id" Json.Decode.int
        |> Json.Decode.Pipeline.required "state" Json.Decode.string
        |> Json.Decode.Pipeline.required "url" Json.Decode.string
        |> Json.Decode.Pipeline.required "width" Json.Decode.int


postThingyAttributesPostMetadataDecoder : Json.Decode.Decoder PostThingyAttributesPostMetadata
postThingyAttributesPostMetadataDecoder =
    Json.Decode.succeed PostThingyAttributesPostMetadata
        |> Json.Decode.Pipeline.required "image_order" (Json.Decode.list Json.Decode.string)
        |> Json.Decode.Pipeline.required "platform" postThingyAttributesPostMetadataPlatformDecoder


postThingyAttributesPostMetadataPlatformDecoder : Json.Decode.Decoder PostThingyAttributesPostMetadataPlatform
postThingyAttributesPostMetadataPlatformDecoder =
    Json.Decode.succeed PostThingyAttributesPostMetadataPlatform


postThingyAttributesThumbnailDecoder : Json.Decode.Decoder PostThingyAttributesThumbnail
postThingyAttributesThumbnailDecoder =
    Json.Decode.succeed PostThingyAttributesThumbnail
        |> Json.Decode.Pipeline.required "large" Json.Decode.string
        |> Json.Decode.Pipeline.required "large_2" Json.Decode.string
        |> Json.Decode.Pipeline.required "square" Json.Decode.string
        |> Json.Decode.Pipeline.required "url" Json.Decode.string


postThingyRelationshipsDecoder : Json.Decode.Decoder PostThingyRelationships
postThingyRelationshipsDecoder =
    Json.Decode.succeed PostThingyRelationships
        |> Json.Decode.Pipeline.required "access_rules" postThingyRelationshipsAccessRulesDecoder
        |> Json.Decode.Pipeline.required "campaign" postThingyRelationshipsCampaignDecoder
        |> Json.Decode.Pipeline.required "images" postThingyRelationshipsImagesDecoder
        |> Json.Decode.Pipeline.required "media" postThingyRelationshipsMediaDecoder
        |> Json.Decode.Pipeline.required "user" postThingyRelationshipsUserDecoder


postThingyRelationshipsAccessRulesDecoder : Json.Decode.Decoder PostThingyRelationshipsAccessRules
postThingyRelationshipsAccessRulesDecoder =
    Json.Decode.succeed PostThingyRelationshipsAccessRules
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list idAndTypeDecoder)


postThingyRelationshipsCampaignDecoder : Json.Decode.Decoder PostThingyRelationshipsCampaign
postThingyRelationshipsCampaignDecoder =
    Json.Decode.succeed PostThingyRelationshipsCampaign
        |> Json.Decode.Pipeline.required "data" idAndTypeDecoder
        |> Json.Decode.Pipeline.required "links" postThingyRelationshipsCampaignLinksDecoder


postThingyRelationshipsCampaignLinksDecoder : Json.Decode.Decoder PostThingyRelationshipsCampaignLinks
postThingyRelationshipsCampaignLinksDecoder =
    Json.Decode.succeed PostThingyRelationshipsCampaignLinks
        |> Json.Decode.Pipeline.required "related" Json.Decode.string


postThingyRelationshipsImagesDecoder : Json.Decode.Decoder PostThingyRelationshipsImages
postThingyRelationshipsImagesDecoder =
    Json.Decode.succeed PostThingyRelationshipsImages
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list idAndTypeDecoder)


postThingyRelationshipsMediaDecoder : Json.Decode.Decoder PostThingyRelationshipsMedia
postThingyRelationshipsMediaDecoder =
    Json.Decode.succeed PostThingyRelationshipsMedia
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list idAndTypeDecoder)


postThingyRelationshipsUserDecoder : Json.Decode.Decoder PostThingyRelationshipsUser
postThingyRelationshipsUserDecoder =
    Json.Decode.succeed PostThingyRelationshipsUser
        |> Json.Decode.Pipeline.required "data" idAndTypeDecoder
        |> Json.Decode.Pipeline.required "links" postThingyRelationshipsUserLinksDecoder


postThingyRelationshipsUserLinksDecoder : Json.Decode.Decoder PostThingyRelationshipsUserLinks
postThingyRelationshipsUserLinksDecoder =
    Json.Decode.succeed PostThingyRelationshipsUserLinks
        |> Json.Decode.Pipeline.required "related" Json.Decode.string


postThingamajigDecoder : Json.Decode.Decoder PostThingamajig
postThingamajigDecoder =
    Json.Decode.succeed PostThingamajig
        |> Json.Decode.Pipeline.required "attributes" postThingamajigAttributesDecoder
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "relationships" postThingamajigRelationshipsDecoder
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postThingamajigAttributesDecoder : Json.Decode.Decoder PostThingamajigAttributes
postThingamajigAttributesDecoder =
    Json.Decode.succeed PostThingamajigAttributes
        |> Json.Decode.Pipeline.required "comment_count" Json.Decode.int
        |> Json.Decode.Pipeline.required "commenter_count" Json.Decode.int
        |> Json.Decode.Pipeline.required "content" Json.Decode.string
        |> Json.Decode.Pipeline.required "created_at" Json.Decode.string
        |> Json.Decode.Pipeline.required "current_user_can_comment" Json.Decode.bool
        |> Json.Decode.Pipeline.required "current_user_can_delete" Json.Decode.bool
        |> Json.Decode.Pipeline.required "current_user_can_report" Json.Decode.bool
        |> Json.Decode.Pipeline.required "current_user_can_view" Json.Decode.bool
        |> Json.Decode.Pipeline.required "current_user_has_liked" Json.Decode.bool
        |> Json.Decode.Pipeline.required "has_ti_violation" Json.Decode.bool
        |> Json.Decode.Pipeline.required "image" imageDecoder
        |> Json.Decode.Pipeline.required "is_new_to_current_user" Json.Decode.bool
        |> Json.Decode.Pipeline.required "is_paid" Json.Decode.bool
        |> Json.Decode.Pipeline.required "like_count" Json.Decode.int
        |> Json.Decode.Pipeline.required "meta_image_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "moderation_status" Json.Decode.string
        |> Json.Decode.Pipeline.required "patreon_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "pledge_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "post_file" postFileDecoder
        |> Json.Decode.Pipeline.required "post_metadata" postThingamajigAttributesPostMetadataDecoder
        |> Json.Decode.Pipeline.required "post_type" Json.Decode.string
        |> Json.Decode.Pipeline.required "preview_asset_type" Json.Decode.string
        |> Json.Decode.Pipeline.required "published_at" Json.Decode.string
        |> Json.Decode.Pipeline.required "teaser_text" Json.Decode.string
        |> Json.Decode.Pipeline.required "thumbnail" postThingamajigAttributesThumbnailDecoder
        |> Json.Decode.Pipeline.required "title" Json.Decode.string
        |> Json.Decode.Pipeline.required "upgrade_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "url" Json.Decode.string
        |> Json.Decode.Pipeline.required "was_posted_by_campaign_owner" Json.Decode.bool


postThingamajigAttributesPostMetadataDecoder : Json.Decode.Decoder PostThingamajigAttributesPostMetadata
postThingamajigAttributesPostMetadataDecoder =
    Json.Decode.succeed PostThingamajigAttributesPostMetadata
        |> Json.Decode.Pipeline.required "image_order" (Json.Decode.list Json.Decode.string)
        |> Json.Decode.Pipeline.required "platform" postThingamajigAttributesPostMetadataPlatformDecoder


postThingamajigAttributesPostMetadataPlatformDecoder : Json.Decode.Decoder PostThingamajigAttributesPostMetadataPlatform
postThingamajigAttributesPostMetadataPlatformDecoder =
    Json.Decode.succeed PostThingamajigAttributesPostMetadataPlatform


postThingamajigAttributesThumbnailDecoder : Json.Decode.Decoder PostThingamajigAttributesThumbnail
postThingamajigAttributesThumbnailDecoder =
    Json.Decode.succeed PostThingamajigAttributesThumbnail
        |> Json.Decode.Pipeline.required "large" Json.Decode.string
        |> Json.Decode.Pipeline.required "large_2" Json.Decode.string
        |> Json.Decode.Pipeline.required "square" Json.Decode.string
        |> Json.Decode.Pipeline.required "url" Json.Decode.string


postThingamajigRelationshipsDecoder : Json.Decode.Decoder PostThingamajigRelationships
postThingamajigRelationshipsDecoder =
    Json.Decode.succeed PostThingamajigRelationships
        |> Json.Decode.Pipeline.required "access_rules" postThingamajigRelationshipsAccessRulesDecoder
        |> Json.Decode.Pipeline.required "attachments_media" postThingamajigRelationshipsAttachmentsMediaDecoder
        |> Json.Decode.Pipeline.required "audio" postThingamajigRelationshipsAudioDecoder
        |> Json.Decode.Pipeline.required "audio_preview" postThingamajigRelationshipsAudioPreviewDecoder
        |> Json.Decode.Pipeline.required "campaign" postThingamajigRelationshipsCampaignDecoder
        |> Json.Decode.Pipeline.required "images" postThingamajigRelationshipsImagesDecoder
        |> Json.Decode.Pipeline.required "media" postThingamajigRelationshipsMediaDecoder
        |> Json.Decode.Pipeline.required "user" postThingamajigRelationshipsUserDecoder


postThingamajigRelationshipsAccessRulesDecoder : Json.Decode.Decoder PostThingamajigRelationshipsAccessRules
postThingamajigRelationshipsAccessRulesDecoder =
    Json.Decode.succeed PostThingamajigRelationshipsAccessRules
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list idAndTypeDecoder)


postThingamajigRelationshipsAttachmentsMediaDecoder : Json.Decode.Decoder PostThingamajigRelationshipsAttachmentsMedia
postThingamajigRelationshipsAttachmentsMediaDecoder =
    Json.Decode.succeed PostThingamajigRelationshipsAttachmentsMedia
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list idAndTypeDecoder)


postThingamajigRelationshipsAudioDecoder : Json.Decode.Decoder PostThingamajigRelationshipsAudio
postThingamajigRelationshipsAudioDecoder =
    Json.Decode.succeed PostThingamajigRelationshipsAudio
        |> Json.Decode.Pipeline.required "data" idAndTypeDecoder
        |> Json.Decode.Pipeline.required "links" postThingamajigRelationshipsAudioLinksDecoder


postThingamajigRelationshipsAudioLinksDecoder : Json.Decode.Decoder PostThingamajigRelationshipsAudioLinks
postThingamajigRelationshipsAudioLinksDecoder =
    Json.Decode.succeed PostThingamajigRelationshipsAudioLinks
        |> Json.Decode.Pipeline.required "related" Json.Decode.string


postThingamajigRelationshipsAudioPreviewDecoder : Json.Decode.Decoder PostThingamajigRelationshipsAudioPreview
postThingamajigRelationshipsAudioPreviewDecoder =
    Json.Decode.succeed PostThingamajigRelationshipsAudioPreview
        |> Json.Decode.Pipeline.required "data" idAndTypeDecoder
        |> Json.Decode.Pipeline.required "links" postThingamajigRelationshipsAudioPreviewLinksDecoder


postThingamajigRelationshipsAudioPreviewLinksDecoder : Json.Decode.Decoder PostThingamajigRelationshipsAudioPreviewLinks
postThingamajigRelationshipsAudioPreviewLinksDecoder =
    Json.Decode.succeed PostThingamajigRelationshipsAudioPreviewLinks
        |> Json.Decode.Pipeline.required "related" Json.Decode.string


postThingamajigRelationshipsCampaignDecoder : Json.Decode.Decoder PostThingamajigRelationshipsCampaign
postThingamajigRelationshipsCampaignDecoder =
    Json.Decode.succeed PostThingamajigRelationshipsCampaign
        |> Json.Decode.Pipeline.required "data" idAndTypeDecoder
        |> Json.Decode.Pipeline.required "links" postThingamajigRelationshipsCampaignLinksDecoder


postThingamajigRelationshipsCampaignLinksDecoder : Json.Decode.Decoder PostThingamajigRelationshipsCampaignLinks
postThingamajigRelationshipsCampaignLinksDecoder =
    Json.Decode.succeed PostThingamajigRelationshipsCampaignLinks
        |> Json.Decode.Pipeline.required "related" Json.Decode.string


postThingamajigRelationshipsImagesDecoder : Json.Decode.Decoder PostThingamajigRelationshipsImages
postThingamajigRelationshipsImagesDecoder =
    Json.Decode.succeed PostThingamajigRelationshipsImages
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list idAndTypeDecoder)


postThingamajigRelationshipsMediaDecoder : Json.Decode.Decoder PostThingamajigRelationshipsMedia
postThingamajigRelationshipsMediaDecoder =
    Json.Decode.succeed PostThingamajigRelationshipsMedia
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list idAndTypeDecoder)


postThingamajigRelationshipsUserDecoder : Json.Decode.Decoder PostThingamajigRelationshipsUser
postThingamajigRelationshipsUserDecoder =
    Json.Decode.succeed PostThingamajigRelationshipsUser
        |> Json.Decode.Pipeline.required "data" idAndTypeDecoder
        |> Json.Decode.Pipeline.required "links" postThingamajigRelationshipsUserLinksDecoder


postThingamajigRelationshipsUserLinksDecoder : Json.Decode.Decoder PostThingamajigRelationshipsUserLinks
postThingamajigRelationshipsUserLinksDecoder =
    Json.Decode.succeed PostThingamajigRelationshipsUserLinks
        |> Json.Decode.Pipeline.required "related" Json.Decode.string


postWhatsitDecoder : Json.Decode.Decoder PostWhatsit
postWhatsitDecoder =
    Json.Decode.succeed PostWhatsit
        |> Json.Decode.Pipeline.required "attributes" postWhatsitAttributesDecoder
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "relationships" postWhatsitRelationshipsDecoder
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postWhatsitAttributesDecoder : Json.Decode.Decoder PostWhatsitAttributes
postWhatsitAttributesDecoder =
    Json.Decode.succeed PostWhatsitAttributes
        |> Json.Decode.Pipeline.required "comment_count" Json.Decode.int
        |> Json.Decode.Pipeline.required "commenter_count" Json.Decode.int
        |> Json.Decode.Pipeline.required "content" Json.Decode.string
        |> Json.Decode.Pipeline.required "created_at" Json.Decode.string
        |> Json.Decode.Pipeline.required "current_user_can_comment" Json.Decode.bool
        |> Json.Decode.Pipeline.required "current_user_can_delete" Json.Decode.bool
        |> Json.Decode.Pipeline.required "current_user_can_report" Json.Decode.bool
        |> Json.Decode.Pipeline.required "current_user_can_view" Json.Decode.bool
        |> Json.Decode.Pipeline.required "current_user_has_liked" Json.Decode.bool
        |> Json.Decode.Pipeline.required "has_ti_violation" Json.Decode.bool
        |> Json.Decode.Pipeline.required "image" imageDecoder
        |> Json.Decode.Pipeline.required "is_new_to_current_user" Json.Decode.bool
        |> Json.Decode.Pipeline.required "is_paid" Json.Decode.bool
        |> Json.Decode.Pipeline.required "like_count" Json.Decode.int
        |> Json.Decode.Pipeline.required "meta_image_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "moderation_status" Json.Decode.string
        |> Json.Decode.Pipeline.required "patreon_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "pledge_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "post_file" postWhatsitAttributesPostFileDecoder
        |> Json.Decode.Pipeline.required "post_metadata" postWhatsitAttributesPostMetadataDecoder
        |> Json.Decode.Pipeline.required "post_type" Json.Decode.string
        |> Json.Decode.Pipeline.required "preview_asset_type" Json.Decode.string
        |> Json.Decode.Pipeline.required "published_at" Json.Decode.string
        |> Json.Decode.Pipeline.required "teaser_text" Json.Decode.string
        |> Json.Decode.Pipeline.required "thumbnail" postWhatsitAttributesThumbnailDecoder
        |> Json.Decode.Pipeline.required "title" Json.Decode.string
        |> Json.Decode.Pipeline.required "upgrade_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "url" Json.Decode.string
        |> Json.Decode.Pipeline.required "was_posted_by_campaign_owner" Json.Decode.bool


postWhatsitAttributesPostFileDecoder : Json.Decode.Decoder PostWhatsitAttributesPostFile
postWhatsitAttributesPostFileDecoder =
    Json.Decode.succeed PostWhatsitAttributesPostFile
        |> Json.Decode.Pipeline.required "height" Json.Decode.int
        |> Json.Decode.Pipeline.required "image_colors" imageColorsDecoder
        |> Json.Decode.Pipeline.required "media_id" Json.Decode.int
        |> Json.Decode.Pipeline.required "state" Json.Decode.string
        |> Json.Decode.Pipeline.required "url" Json.Decode.string
        |> Json.Decode.Pipeline.required "width" Json.Decode.int


postWhatsitAttributesPostMetadataDecoder : Json.Decode.Decoder PostWhatsitAttributesPostMetadata
postWhatsitAttributesPostMetadataDecoder =
    Json.Decode.succeed PostWhatsitAttributesPostMetadata
        |> Json.Decode.Pipeline.required "image_order" (Json.Decode.list Json.Decode.string)
        |> Json.Decode.Pipeline.required "platform" postWhatsitAttributesPostMetadataPlatformDecoder


postWhatsitAttributesPostMetadataPlatformDecoder : Json.Decode.Decoder PostWhatsitAttributesPostMetadataPlatform
postWhatsitAttributesPostMetadataPlatformDecoder =
    Json.Decode.succeed PostWhatsitAttributesPostMetadataPlatform


postWhatsitAttributesThumbnailDecoder : Json.Decode.Decoder PostWhatsitAttributesThumbnail
postWhatsitAttributesThumbnailDecoder =
    Json.Decode.succeed PostWhatsitAttributesThumbnail
        |> Json.Decode.Pipeline.required "large" Json.Decode.string
        |> Json.Decode.Pipeline.required "large_2" Json.Decode.string
        |> Json.Decode.Pipeline.required "square" Json.Decode.string
        |> Json.Decode.Pipeline.required "url" Json.Decode.string


postWhatsitRelationshipsDecoder : Json.Decode.Decoder PostWhatsitRelationships
postWhatsitRelationshipsDecoder =
    Json.Decode.succeed PostWhatsitRelationships
        |> Json.Decode.Pipeline.required "access_rules" postWhatsitRelationshipsAccessRulesDecoder
        |> Json.Decode.Pipeline.required "campaign" postWhatsitRelationshipsCampaignDecoder
        |> Json.Decode.Pipeline.required "images" postWhatsitRelationshipsImagesDecoder
        |> Json.Decode.Pipeline.required "media" postWhatsitRelationshipsMediaDecoder
        |> Json.Decode.Pipeline.required "user" postWhatsitRelationshipsUserDecoder


postWhatsitRelationshipsAccessRulesDecoder : Json.Decode.Decoder PostWhatsitRelationshipsAccessRules
postWhatsitRelationshipsAccessRulesDecoder =
    Json.Decode.succeed PostWhatsitRelationshipsAccessRules
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list idAndTypeDecoder)


postWhatsitRelationshipsCampaignDecoder : Json.Decode.Decoder PostWhatsitRelationshipsCampaign
postWhatsitRelationshipsCampaignDecoder =
    Json.Decode.succeed PostWhatsitRelationshipsCampaign
        |> Json.Decode.Pipeline.required "data" idAndTypeDecoder
        |> Json.Decode.Pipeline.required "links" postWhatsitRelationshipsCampaignLinksDecoder


postWhatsitRelationshipsCampaignLinksDecoder : Json.Decode.Decoder PostWhatsitRelationshipsCampaignLinks
postWhatsitRelationshipsCampaignLinksDecoder =
    Json.Decode.succeed PostWhatsitRelationshipsCampaignLinks
        |> Json.Decode.Pipeline.required "related" Json.Decode.string


postWhatsitRelationshipsImagesDecoder : Json.Decode.Decoder PostWhatsitRelationshipsImages
postWhatsitRelationshipsImagesDecoder =
    Json.Decode.succeed PostWhatsitRelationshipsImages
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list idAndTypeDecoder)


postWhatsitRelationshipsMediaDecoder : Json.Decode.Decoder PostWhatsitRelationshipsMedia
postWhatsitRelationshipsMediaDecoder =
    Json.Decode.succeed PostWhatsitRelationshipsMedia
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list idAndTypeDecoder)


postWhatsitRelationshipsUserDecoder : Json.Decode.Decoder PostWhatsitRelationshipsUser
postWhatsitRelationshipsUserDecoder =
    Json.Decode.succeed PostWhatsitRelationshipsUser
        |> Json.Decode.Pipeline.required "data" idAndTypeDecoder
        |> Json.Decode.Pipeline.required "links" postWhatsitRelationshipsUserLinksDecoder


postWhatsitRelationshipsUserLinksDecoder : Json.Decode.Decoder PostWhatsitRelationshipsUserLinks
postWhatsitRelationshipsUserLinksDecoder =
    Json.Decode.succeed PostWhatsitRelationshipsUserLinks
        |> Json.Decode.Pipeline.required "related" Json.Decode.string


postDoodadDecoder : Json.Decode.Decoder PostDoodad
postDoodadDecoder =
    Json.Decode.succeed PostDoodad
        |> Json.Decode.Pipeline.required "attributes" postDoodadAttributesDecoder
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "relationships" postDoodadRelationshipsDecoder
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postDoodadAttributesDecoder : Json.Decode.Decoder PostDoodadAttributes
postDoodadAttributesDecoder =
    Json.Decode.succeed PostDoodadAttributes
        |> Json.Decode.Pipeline.required "comment_count" Json.Decode.int
        |> Json.Decode.Pipeline.required "commenter_count" Json.Decode.int
        |> Json.Decode.Pipeline.required "content" Json.Decode.string
        |> Json.Decode.Pipeline.required "created_at" Json.Decode.string
        |> Json.Decode.Pipeline.required "current_user_can_comment" Json.Decode.bool
        |> Json.Decode.Pipeline.required "current_user_can_delete" Json.Decode.bool
        |> Json.Decode.Pipeline.required "current_user_can_report" Json.Decode.bool
        |> Json.Decode.Pipeline.required "current_user_can_view" Json.Decode.bool
        |> Json.Decode.Pipeline.required "current_user_has_liked" Json.Decode.bool
        |> Json.Decode.Pipeline.required "has_ti_violation" Json.Decode.bool
        |> Json.Decode.Pipeline.required "image" imageDecoder
        |> Json.Decode.Pipeline.required "is_new_to_current_user" Json.Decode.bool
        |> Json.Decode.Pipeline.required "is_paid" Json.Decode.bool
        |> Json.Decode.Pipeline.required "like_count" Json.Decode.int
        |> Json.Decode.Pipeline.required "meta_image_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "min_cents_pledged_to_view" Json.Decode.int
        |> Json.Decode.Pipeline.required "moderation_status" Json.Decode.string
        |> Json.Decode.Pipeline.required "patreon_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "pledge_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "post_file" postDoodadAttributesPostFileDecoder
        |> Json.Decode.Pipeline.required "post_metadata" postDoodadAttributesPostMetadataDecoder
        |> Json.Decode.Pipeline.required "post_type" Json.Decode.string
        |> Json.Decode.Pipeline.required "preview_asset_type" Json.Decode.string
        |> Json.Decode.Pipeline.required "published_at" Json.Decode.string
        |> Json.Decode.Pipeline.required "teaser_text" Json.Decode.string
        |> Json.Decode.Pipeline.required "thumbnail" postDoodadAttributesThumbnailDecoder
        |> Json.Decode.Pipeline.required "title" Json.Decode.string
        |> Json.Decode.Pipeline.required "upgrade_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "url" Json.Decode.string
        |> Json.Decode.Pipeline.required "was_posted_by_campaign_owner" Json.Decode.bool


postDoodadAttributesPostFileDecoder : Json.Decode.Decoder PostDoodadAttributesPostFile
postDoodadAttributesPostFileDecoder =
    Json.Decode.succeed PostDoodadAttributesPostFile
        |> Json.Decode.Pipeline.required "height" Json.Decode.int
        |> Json.Decode.Pipeline.required "image_colors" imageColorsDecoder
        |> Json.Decode.Pipeline.required "media_id" Json.Decode.int
        |> Json.Decode.Pipeline.required "state" Json.Decode.string
        |> Json.Decode.Pipeline.required "url" Json.Decode.string
        |> Json.Decode.Pipeline.required "width" Json.Decode.int


postDoodadAttributesPostMetadataDecoder : Json.Decode.Decoder PostDoodadAttributesPostMetadata
postDoodadAttributesPostMetadataDecoder =
    Json.Decode.succeed PostDoodadAttributesPostMetadata
        |> Json.Decode.Pipeline.required "image_order" (Json.Decode.list Json.Decode.string)
        |> Json.Decode.Pipeline.required "platform" postDoodadAttributesPostMetadataPlatformDecoder


postDoodadAttributesPostMetadataPlatformDecoder : Json.Decode.Decoder PostDoodadAttributesPostMetadataPlatform
postDoodadAttributesPostMetadataPlatformDecoder =
    Json.Decode.succeed PostDoodadAttributesPostMetadataPlatform


postDoodadAttributesThumbnailDecoder : Json.Decode.Decoder PostDoodadAttributesThumbnail
postDoodadAttributesThumbnailDecoder =
    Json.Decode.succeed PostDoodadAttributesThumbnail
        |> Json.Decode.Pipeline.required "large" Json.Decode.string
        |> Json.Decode.Pipeline.required "large_2" Json.Decode.string
        |> Json.Decode.Pipeline.required "square" Json.Decode.string
        |> Json.Decode.Pipeline.required "url" Json.Decode.string


postDoodadRelationshipsDecoder : Json.Decode.Decoder PostDoodadRelationships
postDoodadRelationshipsDecoder =
    Json.Decode.succeed PostDoodadRelationships
        |> Json.Decode.Pipeline.required "access_rules" postDoodadRelationshipsAccessRulesDecoder
        |> Json.Decode.Pipeline.required "campaign" postDoodadRelationshipsCampaignDecoder
        |> Json.Decode.Pipeline.required "images" postDoodadRelationshipsImagesDecoder
        |> Json.Decode.Pipeline.required "media" postDoodadRelationshipsMediaDecoder
        |> Json.Decode.Pipeline.required "user" postDoodadRelationshipsUserDecoder


postDoodadRelationshipsAccessRulesDecoder : Json.Decode.Decoder PostDoodadRelationshipsAccessRules
postDoodadRelationshipsAccessRulesDecoder =
    Json.Decode.succeed PostDoodadRelationshipsAccessRules
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list idAndTypeDecoder)


postDoodadRelationshipsCampaignDecoder : Json.Decode.Decoder PostDoodadRelationshipsCampaign
postDoodadRelationshipsCampaignDecoder =
    Json.Decode.succeed PostDoodadRelationshipsCampaign
        |> Json.Decode.Pipeline.required "data" idAndTypeDecoder
        |> Json.Decode.Pipeline.required "links" postDoodadRelationshipsCampaignLinksDecoder


postDoodadRelationshipsCampaignLinksDecoder : Json.Decode.Decoder PostDoodadRelationshipsCampaignLinks
postDoodadRelationshipsCampaignLinksDecoder =
    Json.Decode.succeed PostDoodadRelationshipsCampaignLinks
        |> Json.Decode.Pipeline.required "related" Json.Decode.string


postDoodadRelationshipsImagesDecoder : Json.Decode.Decoder PostDoodadRelationshipsImages
postDoodadRelationshipsImagesDecoder =
    Json.Decode.succeed PostDoodadRelationshipsImages
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list idAndTypeDecoder)


postDoodadRelationshipsMediaDecoder : Json.Decode.Decoder PostDoodadRelationshipsMedia
postDoodadRelationshipsMediaDecoder =
    Json.Decode.succeed PostDoodadRelationshipsMedia
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list idAndTypeDecoder)


postDoodadRelationshipsUserDecoder : Json.Decode.Decoder PostDoodadRelationshipsUser
postDoodadRelationshipsUserDecoder =
    Json.Decode.succeed PostDoodadRelationshipsUser
        |> Json.Decode.Pipeline.required "data" idAndTypeDecoder
        |> Json.Decode.Pipeline.required "links" postDoodadRelationshipsUserLinksDecoder


postDoodadRelationshipsUserLinksDecoder : Json.Decode.Decoder PostDoodadRelationshipsUserLinks
postDoodadRelationshipsUserLinksDecoder =
    Json.Decode.succeed PostDoodadRelationshipsUserLinks
        |> Json.Decode.Pipeline.required "related" Json.Decode.string
