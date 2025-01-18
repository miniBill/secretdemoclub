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
    { id : String
    , type_ : String
    }


type alias PostObjectRelationshipsAudio =
    { data : PostObjectRelationshipsAudioData
    , links : PostObjectRelationshipsAudioLinks
    }


type alias PostObjectRelationshipsAudioData =
    { id : String
    , type_ : String
    }


type alias PostObjectRelationshipsAudioLinks =
    { related : String
    }


type alias PostObjectRelationshipsCampaign =
    { data : PostObjectRelationshipsCampaignData
    , links : PostObjectRelationshipsCampaignLinks
    }


type alias PostObjectRelationshipsCampaignData =
    { id : String
    , type_ : String
    }


type alias PostObjectRelationshipsCampaignLinks =
    { related : String
    }


type alias PostObjectRelationshipsImages =
    { data : List PostObjectRelationshipsImagesDataObject
    }


type alias PostObjectRelationshipsImagesDataObject =
    { id : String
    , type_ : String
    }


type alias PostObjectRelationshipsMedia =
    { data : List PostObjectRelationshipsMediaDataObject
    }


type alias PostObjectRelationshipsMediaDataObject =
    { id : String
    , type_ : String
    }


type alias PostObjectRelationshipsUser =
    { data : PostObjectRelationshipsUserData
    , links : PostObjectRelationshipsUserLinks
    }


type alias PostObjectRelationshipsUserData =
    { id : String
    , type_ : String
    }


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
    , imageColors : PostMemberAttributesPostFileImageColors
    , mediaId : Int
    , state : String
    , url : String
    , width : Int
    }


type alias PostMemberAttributesPostFileImageColors =
    { averageColorsOfCorners : PostMemberAttributesPostFileImageColorsAverageColorsOfCorners
    , dominantColor : String
    , palette : List String
    , textColor : String
    }


type alias PostMemberAttributesPostFileImageColorsAverageColorsOfCorners =
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
    { id : String
    , type_ : String
    }


type alias PostMemberRelationshipsCampaign =
    { data : PostMemberRelationshipsCampaignData
    , links : PostMemberRelationshipsCampaignLinks
    }


type alias PostMemberRelationshipsCampaignData =
    { id : String
    , type_ : String
    }


type alias PostMemberRelationshipsCampaignLinks =
    { related : String
    }


type alias PostMemberRelationshipsImages =
    { data : List PostMemberRelationshipsImagesDataObject
    }


type alias PostMemberRelationshipsImagesDataObject =
    { id : String
    , type_ : String
    }


type alias PostMemberRelationshipsMedia =
    { data : List PostMemberRelationshipsMediaDataObject
    }


type alias PostMemberRelationshipsMediaDataObject =
    { id : String
    , type_ : String
    }


type alias PostMemberRelationshipsUser =
    { data : PostMemberRelationshipsUserData
    , links : PostMemberRelationshipsUserLinks
    }


type alias PostMemberRelationshipsUserData =
    { id : String
    , type_ : String
    }


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
    { id : String
    , type_ : String
    }


type alias PostEntityRelationshipsCampaign =
    { data : PostEntityRelationshipsCampaignData
    , links : PostEntityRelationshipsCampaignLinks
    }


type alias PostEntityRelationshipsCampaignData =
    { id : String
    , type_ : String
    }


type alias PostEntityRelationshipsCampaignLinks =
    { related : String
    }


type alias PostEntityRelationshipsUser =
    { data : PostEntityRelationshipsUserData
    , links : PostEntityRelationshipsUserLinks
    }


type alias PostEntityRelationshipsUserData =
    { id : String
    , type_ : String
    }


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
    { defaultThumbnail : HasUrl
    , duration : Int
    , fullContentDuration : Int
    , mediaId : Int
    , progress : Progress
    , state : String
    , url : String
    }


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
    { id : String
    , type_ : String
    }


type alias PostThingRelationshipsAttachmentsMedia =
    { data : List PostThingRelationshipsAttachmentsMediaDataObject
    }


type alias PostThingRelationshipsAttachmentsMediaDataObject =
    { id : String
    , type_ : String
    }


type alias PostThingRelationshipsAudio =
    { data : PostThingRelationshipsAudioData
    , links : PostThingRelationshipsAudioLinks
    }


type alias PostThingRelationshipsAudioData =
    { id : String
    , type_ : String
    }


type alias PostThingRelationshipsAudioLinks =
    { related : String
    }


type alias PostThingRelationshipsCampaign =
    { data : PostThingRelationshipsCampaignData
    , links : PostThingRelationshipsCampaignLinks
    }


type alias PostThingRelationshipsCampaignData =
    { id : String
    , type_ : String
    }


type alias PostThingRelationshipsCampaignLinks =
    { related : String
    }


type alias PostThingRelationshipsImages =
    { data : List PostThingRelationshipsImagesDataObject
    }


type alias PostThingRelationshipsImagesDataObject =
    { id : String
    , type_ : String
    }


type alias PostThingRelationshipsMedia =
    { data : List PostThingRelationshipsMediaDataObject
    }


type alias PostThingRelationshipsMediaDataObject =
    { id : String
    , type_ : String
    }


type alias PostThingRelationshipsUser =
    { data : PostThingRelationshipsUserData
    , links : PostThingRelationshipsUserLinks
    }


type alias PostThingRelationshipsUserData =
    { id : String
    , type_ : String
    }


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
    { averageColorsOfCorners : PostInstanceAttributesPostFileImageColorsAverageColorsOfCorners
    , dominantColor : String
    , palette : List String
    , textColor : String
    }


type alias PostInstanceAttributesPostFileImageColorsAverageColorsOfCorners =
    { bottomLeft : String
    , bottomRight : String
    , topLeft : String
    , topRight : String
    }


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
    { id : String
    , type_ : String
    }


type alias PostInstanceRelationshipsCampaign =
    { data : PostInstanceRelationshipsCampaignData
    , links : PostInstanceRelationshipsCampaignLinks
    }


type alias PostInstanceRelationshipsCampaignData =
    { id : String
    , type_ : String
    }


type alias PostInstanceRelationshipsCampaignLinks =
    { related : String
    }


type alias PostInstanceRelationshipsImages =
    { data : List PostInstanceRelationshipsImagesDataObject
    }


type alias PostInstanceRelationshipsImagesDataObject =
    { id : String
    , type_ : String
    }


type alias PostInstanceRelationshipsMedia =
    { data : List PostInstanceRelationshipsMediaDataObject
    }


type alias PostInstanceRelationshipsMediaDataObject =
    { id : String
    , type_ : String
    }


type alias PostInstanceRelationshipsUser =
    { data : PostInstanceRelationshipsUserData
    , links : PostInstanceRelationshipsUserLinks
    }


type alias PostInstanceRelationshipsUserData =
    { id : String
    , type_ : String
    }


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
    { averageColorsOfCorners : PostConstituentAttributesPostFileImageColorsAverageColorsOfCorners
    , dominantColor : String
    , palette : List String
    , textColor : String
    }


type alias PostConstituentAttributesPostFileImageColorsAverageColorsOfCorners =
    { bottomLeft : String
    , bottomRight : String
    , topLeft : String
    , topRight : String
    }


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
    { id : String
    , type_ : String
    }


type alias PostConstituentRelationshipsCampaign =
    { data : PostConstituentRelationshipsCampaignData
    , links : PostConstituentRelationshipsCampaignLinks
    }


type alias PostConstituentRelationshipsCampaignData =
    { id : String
    , type_ : String
    }


type alias PostConstituentRelationshipsCampaignLinks =
    { related : String
    }


type alias PostConstituentRelationshipsImages =
    { data : List PostConstituentRelationshipsImagesDataObject
    }


type alias PostConstituentRelationshipsImagesDataObject =
    { id : String
    , type_ : String
    }


type alias PostConstituentRelationshipsMedia =
    { data : List PostConstituentRelationshipsMediaDataObject
    }


type alias PostConstituentRelationshipsMediaDataObject =
    { id : String
    , type_ : String
    }


type alias PostConstituentRelationshipsUser =
    { data : PostConstituentRelationshipsUserData
    , links : PostConstituentRelationshipsUserLinks
    }


type alias PostConstituentRelationshipsUserData =
    { id : String
    , type_ : String
    }


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
    { averageColorsOfCorners : PostSpecimenAttributesPostFileImageColorsAverageColorsOfCorners
    , dominantColor : String
    , palette : List String
    , textColor : String
    }


type alias PostSpecimenAttributesPostFileImageColorsAverageColorsOfCorners =
    { bottomLeft : String
    , bottomRight : String
    , topLeft : String
    , topRight : String
    }


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
    { id : String
    , type_ : String
    }


type alias PostSpecimenRelationshipsCampaign =
    { data : PostSpecimenRelationshipsCampaignData
    , links : PostSpecimenRelationshipsCampaignLinks
    }


type alias PostSpecimenRelationshipsCampaignData =
    { id : String
    , type_ : String
    }


type alias PostSpecimenRelationshipsCampaignLinks =
    { related : String
    }


type alias PostSpecimenRelationshipsImages =
    { data : List PostSpecimenRelationshipsImagesDataObject
    }


type alias PostSpecimenRelationshipsImagesDataObject =
    { id : String
    , type_ : String
    }


type alias PostSpecimenRelationshipsMedia =
    { data : List PostSpecimenRelationshipsMediaDataObject
    }


type alias PostSpecimenRelationshipsMediaDataObject =
    { id : String
    , type_ : String
    }


type alias PostSpecimenRelationshipsUser =
    { data : PostSpecimenRelationshipsUserData
    , links : PostSpecimenRelationshipsUserLinks
    }


type alias PostSpecimenRelationshipsUserData =
    { id : String
    , type_ : String
    }


type alias PostSpecimenRelationshipsUserLinks =
    { related : String
    }


type alias PostSpecimenRelationshipsUserDefinedTags =
    { data : List PostSpecimenRelationshipsUserDefinedTagsDataObject
    }


type alias PostSpecimenRelationshipsUserDefinedTagsDataObject =
    { id : String
    , type_ : String
    }


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
    { defaultThumbnail : HasUrl
    , duration : Int
    , fullContentDuration : Int
    , mediaId : Int
    , progress : Progress
    , state : String
    , url : String
    }


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
    { id : String
    , type_ : String
    }


type alias PostGadgetRelationshipsAudio =
    { data : PostGadgetRelationshipsAudioData
    , links : PostGadgetRelationshipsAudioLinks
    }


type alias PostGadgetRelationshipsAudioData =
    { id : String
    , type_ : String
    }


type alias PostGadgetRelationshipsAudioLinks =
    { related : String
    }


type alias PostGadgetRelationshipsCampaign =
    { data : PostGadgetRelationshipsCampaignData
    , links : PostGadgetRelationshipsCampaignLinks
    }


type alias PostGadgetRelationshipsCampaignData =
    { id : String
    , type_ : String
    }


type alias PostGadgetRelationshipsCampaignLinks =
    { related : String
    }


type alias PostGadgetRelationshipsMedia =
    { data : List PostGadgetRelationshipsMediaDataObject
    }


type alias PostGadgetRelationshipsMediaDataObject =
    { id : String
    , type_ : String
    }


type alias PostGadgetRelationshipsUser =
    { data : PostGadgetRelationshipsUserData
    , links : PostGadgetRelationshipsUserLinks
    }


type alias PostGadgetRelationshipsUserData =
    { id : String
    , type_ : String
    }


type alias PostGadgetRelationshipsUserLinks =
    { related : String
    }


type alias PostGadgetRelationshipsUserDefinedTags =
    { data : List PostGadgetRelationshipsUserDefinedTagsDataObject
    }


type alias PostGadgetRelationshipsUserDefinedTagsDataObject =
    { id : String
    , type_ : String
    }


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
    { averageColorsOfCorners : PostWidgetAttributesPostFileImageColorsAverageColorsOfCorners
    , dominantColor : String
    , palette : List String
    , textColor : String
    }


type alias PostWidgetAttributesPostFileImageColorsAverageColorsOfCorners =
    { bottomLeft : String
    , bottomRight : String
    , topLeft : String
    , topRight : String
    }


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
    { id : String
    , type_ : String
    }


type alias PostWidgetRelationshipsCampaign =
    { data : PostWidgetRelationshipsCampaignData
    , links : PostWidgetRelationshipsCampaignLinks
    }


type alias PostWidgetRelationshipsCampaignData =
    { id : String
    , type_ : String
    }


type alias PostWidgetRelationshipsCampaignLinks =
    { related : String
    }


type alias PostWidgetRelationshipsImages =
    { data : List PostWidgetRelationshipsImagesDataObject
    }


type alias PostWidgetRelationshipsImagesDataObject =
    { id : String
    , type_ : String
    }


type alias PostWidgetRelationshipsMedia =
    { data : List PostWidgetRelationshipsMediaDataObject
    }


type alias PostWidgetRelationshipsMediaDataObject =
    { id : String
    , type_ : String
    }


type alias PostWidgetRelationshipsUser =
    { data : PostWidgetRelationshipsUserData
    , links : PostWidgetRelationshipsUserLinks
    }


type alias PostWidgetRelationshipsUserData =
    { id : String
    , type_ : String
    }


type alias PostWidgetRelationshipsUserLinks =
    { related : String
    }


type alias PostWidgetRelationshipsUserDefinedTags =
    { data : List PostWidgetRelationshipsUserDefinedTagsDataObject
    }


type alias PostWidgetRelationshipsUserDefinedTagsDataObject =
    { id : String
    , type_ : String
    }


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
    { defaultThumbnail : HasUrl
    , duration : Int
    , fullContentDuration : Int
    , mediaId : Int
    , progress : Progress
    , state : String
    , url : String
    }


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
    { id : String
    , type_ : String
    }


type alias PostGizmoRelationshipsAudio =
    { data : PostGizmoRelationshipsAudioData
    , links : PostGizmoRelationshipsAudioLinks
    }


type alias PostGizmoRelationshipsAudioData =
    { id : String
    , type_ : String
    }


type alias PostGizmoRelationshipsAudioLinks =
    { related : String
    }


type alias PostGizmoRelationshipsAudioPreview =
    { data : PostGizmoRelationshipsAudioPreviewData
    , links : PostGizmoRelationshipsAudioPreviewLinks
    }


type alias PostGizmoRelationshipsAudioPreviewData =
    { id : String
    , type_ : String
    }


type alias PostGizmoRelationshipsAudioPreviewLinks =
    { related : String
    }


type alias PostGizmoRelationshipsCampaign =
    { data : PostGizmoRelationshipsCampaignData
    , links : PostGizmoRelationshipsCampaignLinks
    }


type alias PostGizmoRelationshipsCampaignData =
    { id : String
    , type_ : String
    }


type alias PostGizmoRelationshipsCampaignLinks =
    { related : String
    }


type alias PostGizmoRelationshipsImages =
    { data : List PostGizmoRelationshipsImagesDataObject
    }


type alias PostGizmoRelationshipsImagesDataObject =
    { id : String
    , type_ : String
    }


type alias PostGizmoRelationshipsMedia =
    { data : List PostGizmoRelationshipsMediaDataObject
    }


type alias PostGizmoRelationshipsMediaDataObject =
    { id : String
    , type_ : String
    }


type alias PostGizmoRelationshipsUser =
    { data : PostGizmoRelationshipsUserData
    , links : PostGizmoRelationshipsUserLinks
    }


type alias PostGizmoRelationshipsUserData =
    { id : String
    , type_ : String
    }


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
    { defaultThumbnail : HasUrl
    , duration : Int
    , fullContentDuration : Int
    , mediaId : Int
    , progress : Progress
    , state : String
    , url : String
    }


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
    { id : String
    , type_ : String
    }


type alias PostPartRelationshipsAttachmentsMedia =
    { data : List PostPartRelationshipsAttachmentsMediaDataObject
    }


type alias PostPartRelationshipsAttachmentsMediaDataObject =
    { id : String
    , type_ : String
    }


type alias PostPartRelationshipsAudio =
    { data : PostPartRelationshipsAudioData
    , links : PostPartRelationshipsAudioLinks
    }


type alias PostPartRelationshipsAudioData =
    { id : String
    , type_ : String
    }


type alias PostPartRelationshipsAudioLinks =
    { related : String
    }


type alias PostPartRelationshipsAudioPreview =
    { data : PostPartRelationshipsAudioPreviewData
    , links : PostPartRelationshipsAudioPreviewLinks
    }


type alias PostPartRelationshipsAudioPreviewData =
    { id : String
    , type_ : String
    }


type alias PostPartRelationshipsAudioPreviewLinks =
    { related : String
    }


type alias PostPartRelationshipsCampaign =
    { data : PostPartRelationshipsCampaignData
    , links : PostPartRelationshipsCampaignLinks
    }


type alias PostPartRelationshipsCampaignData =
    { id : String
    , type_ : String
    }


type alias PostPartRelationshipsCampaignLinks =
    { related : String
    }


type alias PostPartRelationshipsMedia =
    { data : List PostPartRelationshipsMediaDataObject
    }


type alias PostPartRelationshipsMediaDataObject =
    { id : String
    , type_ : String
    }


type alias PostPartRelationshipsUser =
    { data : PostPartRelationshipsUserData
    , links : PostPartRelationshipsUserLinks
    }


type alias PostPartRelationshipsUserData =
    { id : String
    , type_ : String
    }


type alias PostPartRelationshipsUserLinks =
    { related : String
    }


type alias PostPartRelationshipsUserDefinedTags =
    { data : List PostPartRelationshipsUserDefinedTagsDataObject
    }


type alias PostPartRelationshipsUserDefinedTagsDataObject =
    { id : String
    , type_ : String
    }


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
    { averageColorsOfCorners : PostChunkAttributesPostFileImageColorsAverageColorsOfCorners
    , dominantColor : String
    , palette : List String
    , textColor : String
    }


type alias PostChunkAttributesPostFileImageColorsAverageColorsOfCorners =
    { bottomLeft : String
    , bottomRight : String
    , topLeft : String
    , topRight : String
    }


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
    { id : String
    , type_ : String
    }


type alias PostChunkRelationshipsCampaign =
    { data : PostChunkRelationshipsCampaignData
    , links : PostChunkRelationshipsCampaignLinks
    }


type alias PostChunkRelationshipsCampaignData =
    { id : String
    , type_ : String
    }


type alias PostChunkRelationshipsCampaignLinks =
    { related : String
    }


type alias PostChunkRelationshipsImages =
    { data : List PostChunkRelationshipsImagesDataObject
    }


type alias PostChunkRelationshipsImagesDataObject =
    { id : String
    , type_ : String
    }


type alias PostChunkRelationshipsMedia =
    { data : List PostChunkRelationshipsMediaDataObject
    }


type alias PostChunkRelationshipsMediaDataObject =
    { id : String
    , type_ : String
    }


type alias PostChunkRelationshipsUser =
    { data : PostChunkRelationshipsUserData
    , links : PostChunkRelationshipsUserLinks
    }


type alias PostChunkRelationshipsUserData =
    { id : String
    , type_ : String
    }


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
    { averageColorsOfCorners : PostPieceAttributesPostFileImageColorsAverageColorsOfCorners
    , dominantColor : String
    , palette : List String
    , textColor : String
    }


type alias PostPieceAttributesPostFileImageColorsAverageColorsOfCorners =
    { bottomLeft : String
    , bottomRight : String
    , topLeft : String
    , topRight : String
    }


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
    { id : String
    , type_ : String
    }


type alias PostPieceRelationshipsCampaign =
    { data : PostPieceRelationshipsCampaignData
    , links : PostPieceRelationshipsCampaignLinks
    }


type alias PostPieceRelationshipsCampaignData =
    { id : String
    , type_ : String
    }


type alias PostPieceRelationshipsCampaignLinks =
    { related : String
    }


type alias PostPieceRelationshipsImages =
    { data : List PostPieceRelationshipsImagesDataObject
    }


type alias PostPieceRelationshipsImagesDataObject =
    { id : String
    , type_ : String
    }


type alias PostPieceRelationshipsMedia =
    { data : List PostPieceRelationshipsMediaDataObject
    }


type alias PostPieceRelationshipsMediaDataObject =
    { id : String
    , type_ : String
    }


type alias PostPieceRelationshipsUser =
    { data : PostPieceRelationshipsUserData
    , links : PostPieceRelationshipsUserLinks
    }


type alias PostPieceRelationshipsUserData =
    { id : String
    , type_ : String
    }


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
    { averageColorsOfCorners : PostThingyAttributesPostFileImageColorsAverageColorsOfCorners
    , dominantColor : String
    , palette : List String
    , textColor : String
    }


type alias PostThingyAttributesPostFileImageColorsAverageColorsOfCorners =
    { bottomLeft : String
    , bottomRight : String
    , topLeft : String
    , topRight : String
    }


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
    { id : String
    , type_ : String
    }


type alias PostThingyRelationshipsCampaign =
    { data : PostThingyRelationshipsCampaignData
    , links : PostThingyRelationshipsCampaignLinks
    }


type alias PostThingyRelationshipsCampaignData =
    { id : String
    , type_ : String
    }


type alias PostThingyRelationshipsCampaignLinks =
    { related : String
    }


type alias PostThingyRelationshipsImages =
    { data : List PostThingyRelationshipsImagesDataObject
    }


type alias PostThingyRelationshipsImagesDataObject =
    { id : String
    , type_ : String
    }


type alias PostThingyRelationshipsMedia =
    { data : List PostThingyRelationshipsMediaDataObject
    }


type alias PostThingyRelationshipsMediaDataObject =
    { id : String
    , type_ : String
    }


type alias PostThingyRelationshipsUser =
    { data : PostThingyRelationshipsUserData
    , links : PostThingyRelationshipsUserLinks
    }


type alias PostThingyRelationshipsUserData =
    { id : String
    , type_ : String
    }


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
    { defaultThumbnail : HasUrl
    , duration : Int
    , fullContentDuration : Int
    , mediaId : Int
    , progress : Progress
    , state : String
    , url : String
    }


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
    { id : String
    , type_ : String
    }


type alias PostThingamajigRelationshipsAttachmentsMedia =
    { data : List PostThingamajigRelationshipsAttachmentsMediaDataObject
    }


type alias PostThingamajigRelationshipsAttachmentsMediaDataObject =
    { id : String
    , type_ : String
    }


type alias PostThingamajigRelationshipsAudio =
    { data : PostThingamajigRelationshipsAudioData
    , links : PostThingamajigRelationshipsAudioLinks
    }


type alias PostThingamajigRelationshipsAudioData =
    { id : String
    , type_ : String
    }


type alias PostThingamajigRelationshipsAudioLinks =
    { related : String
    }


type alias PostThingamajigRelationshipsAudioPreview =
    { data : PostThingamajigRelationshipsAudioPreviewData
    , links : PostThingamajigRelationshipsAudioPreviewLinks
    }


type alias PostThingamajigRelationshipsAudioPreviewData =
    { id : String
    , type_ : String
    }


type alias PostThingamajigRelationshipsAudioPreviewLinks =
    { related : String
    }


type alias PostThingamajigRelationshipsCampaign =
    { data : PostThingamajigRelationshipsCampaignData
    , links : PostThingamajigRelationshipsCampaignLinks
    }


type alias PostThingamajigRelationshipsCampaignData =
    { id : String
    , type_ : String
    }


type alias PostThingamajigRelationshipsCampaignLinks =
    { related : String
    }


type alias PostThingamajigRelationshipsImages =
    { data : List PostThingamajigRelationshipsImagesDataObject
    }


type alias PostThingamajigRelationshipsImagesDataObject =
    { id : String
    , type_ : String
    }


type alias PostThingamajigRelationshipsMedia =
    { data : List PostThingamajigRelationshipsMediaDataObject
    }


type alias PostThingamajigRelationshipsMediaDataObject =
    { id : String
    , type_ : String
    }


type alias PostThingamajigRelationshipsUser =
    { data : PostThingamajigRelationshipsUserData
    , links : PostThingamajigRelationshipsUserLinks
    }


type alias PostThingamajigRelationshipsUserData =
    { id : String
    , type_ : String
    }


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
    { averageColorsOfCorners : PostWhatsitAttributesPostFileImageColorsAverageColorsOfCorners
    , dominantColor : String
    , palette : List String
    , textColor : String
    }


type alias PostWhatsitAttributesPostFileImageColorsAverageColorsOfCorners =
    { bottomLeft : String
    , bottomRight : String
    , topLeft : String
    , topRight : String
    }


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
    { id : String
    , type_ : String
    }


type alias PostWhatsitRelationshipsCampaign =
    { data : PostWhatsitRelationshipsCampaignData
    , links : PostWhatsitRelationshipsCampaignLinks
    }


type alias PostWhatsitRelationshipsCampaignData =
    { id : String
    , type_ : String
    }


type alias PostWhatsitRelationshipsCampaignLinks =
    { related : String
    }


type alias PostWhatsitRelationshipsImages =
    { data : List PostWhatsitRelationshipsImagesDataObject
    }


type alias PostWhatsitRelationshipsImagesDataObject =
    { id : String
    , type_ : String
    }


type alias PostWhatsitRelationshipsMedia =
    { data : List PostWhatsitRelationshipsMediaDataObject
    }


type alias PostWhatsitRelationshipsMediaDataObject =
    { id : String
    , type_ : String
    }


type alias PostWhatsitRelationshipsUser =
    { data : PostWhatsitRelationshipsUserData
    , links : PostWhatsitRelationshipsUserLinks
    }


type alias PostWhatsitRelationshipsUserData =
    { id : String
    , type_ : String
    }


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
    { averageColorsOfCorners : PostDoodadAttributesPostFileImageColorsAverageColorsOfCorners
    , dominantColor : String
    , palette : List String
    , textColor : String
    }


type alias PostDoodadAttributesPostFileImageColorsAverageColorsOfCorners =
    { bottomLeft : String
    , bottomRight : String
    , topLeft : String
    , topRight : String
    }


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
    { id : String
    , type_ : String
    }


type alias PostDoodadRelationshipsCampaign =
    { data : PostDoodadRelationshipsCampaignData
    , links : PostDoodadRelationshipsCampaignLinks
    }


type alias PostDoodadRelationshipsCampaignData =
    { id : String
    , type_ : String
    }


type alias PostDoodadRelationshipsCampaignLinks =
    { related : String
    }


type alias PostDoodadRelationshipsImages =
    { data : List PostDoodadRelationshipsImagesDataObject
    }


type alias PostDoodadRelationshipsImagesDataObject =
    { id : String
    , type_ : String
    }


type alias PostDoodadRelationshipsMedia =
    { data : List PostDoodadRelationshipsMediaDataObject
    }


type alias PostDoodadRelationshipsMediaDataObject =
    { id : String
    , type_ : String
    }


type alias PostDoodadRelationshipsUser =
    { data : PostDoodadRelationshipsUserData
    , links : PostDoodadRelationshipsUserLinks
    }


type alias PostDoodadRelationshipsUserData =
    { id : String
    , type_ : String
    }


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
        |> Json.Decode.Pipeline.required "post_file" postObjectAttributesPostFileDecoder
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


postObjectAttributesPostFileDecoder : Json.Decode.Decoder PostObjectAttributesPostFile
postObjectAttributesPostFileDecoder =
    Json.Decode.succeed PostObjectAttributesPostFile
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
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list postObjectRelationshipsAccessRulesDataObjectDecoder)


postObjectRelationshipsAccessRulesDataObjectDecoder : Json.Decode.Decoder PostObjectRelationshipsAccessRulesDataObject
postObjectRelationshipsAccessRulesDataObjectDecoder =
    Json.Decode.succeed PostObjectRelationshipsAccessRulesDataObject
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postObjectRelationshipsAudioDecoder : Json.Decode.Decoder PostObjectRelationshipsAudio
postObjectRelationshipsAudioDecoder =
    Json.Decode.succeed PostObjectRelationshipsAudio
        |> Json.Decode.Pipeline.required "data" postObjectRelationshipsAudioDataDecoder
        |> Json.Decode.Pipeline.required "links" postObjectRelationshipsAudioLinksDecoder


postObjectRelationshipsAudioDataDecoder : Json.Decode.Decoder PostObjectRelationshipsAudioData
postObjectRelationshipsAudioDataDecoder =
    Json.Decode.succeed PostObjectRelationshipsAudioData
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postObjectRelationshipsAudioLinksDecoder : Json.Decode.Decoder PostObjectRelationshipsAudioLinks
postObjectRelationshipsAudioLinksDecoder =
    Json.Decode.succeed PostObjectRelationshipsAudioLinks
        |> Json.Decode.Pipeline.required "related" Json.Decode.string


postObjectRelationshipsCampaignDecoder : Json.Decode.Decoder PostObjectRelationshipsCampaign
postObjectRelationshipsCampaignDecoder =
    Json.Decode.succeed PostObjectRelationshipsCampaign
        |> Json.Decode.Pipeline.required "data" postObjectRelationshipsCampaignDataDecoder
        |> Json.Decode.Pipeline.required "links" postObjectRelationshipsCampaignLinksDecoder


postObjectRelationshipsCampaignDataDecoder : Json.Decode.Decoder PostObjectRelationshipsCampaignData
postObjectRelationshipsCampaignDataDecoder =
    Json.Decode.succeed PostObjectRelationshipsCampaignData
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postObjectRelationshipsCampaignLinksDecoder : Json.Decode.Decoder PostObjectRelationshipsCampaignLinks
postObjectRelationshipsCampaignLinksDecoder =
    Json.Decode.succeed PostObjectRelationshipsCampaignLinks
        |> Json.Decode.Pipeline.required "related" Json.Decode.string


postObjectRelationshipsImagesDecoder : Json.Decode.Decoder PostObjectRelationshipsImages
postObjectRelationshipsImagesDecoder =
    Json.Decode.succeed PostObjectRelationshipsImages
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list postObjectRelationshipsImagesDataObjectDecoder)


postObjectRelationshipsImagesDataObjectDecoder : Json.Decode.Decoder PostObjectRelationshipsImagesDataObject
postObjectRelationshipsImagesDataObjectDecoder =
    Json.Decode.succeed PostObjectRelationshipsImagesDataObject
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postObjectRelationshipsMediaDecoder : Json.Decode.Decoder PostObjectRelationshipsMedia
postObjectRelationshipsMediaDecoder =
    Json.Decode.succeed PostObjectRelationshipsMedia
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list postObjectRelationshipsMediaDataObjectDecoder)


postObjectRelationshipsMediaDataObjectDecoder : Json.Decode.Decoder PostObjectRelationshipsMediaDataObject
postObjectRelationshipsMediaDataObjectDecoder =
    Json.Decode.succeed PostObjectRelationshipsMediaDataObject
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postObjectRelationshipsUserDecoder : Json.Decode.Decoder PostObjectRelationshipsUser
postObjectRelationshipsUserDecoder =
    Json.Decode.succeed PostObjectRelationshipsUser
        |> Json.Decode.Pipeline.required "data" postObjectRelationshipsUserDataDecoder
        |> Json.Decode.Pipeline.required "links" postObjectRelationshipsUserLinksDecoder


postObjectRelationshipsUserDataDecoder : Json.Decode.Decoder PostObjectRelationshipsUserData
postObjectRelationshipsUserDataDecoder =
    Json.Decode.succeed PostObjectRelationshipsUserData
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


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
        |> Json.Decode.Pipeline.required "image_colors" postMemberAttributesPostFileImageColorsDecoder
        |> Json.Decode.Pipeline.required "media_id" Json.Decode.int
        |> Json.Decode.Pipeline.required "state" Json.Decode.string
        |> Json.Decode.Pipeline.required "url" Json.Decode.string
        |> Json.Decode.Pipeline.required "width" Json.Decode.int


postMemberAttributesPostFileImageColorsDecoder : Json.Decode.Decoder PostMemberAttributesPostFileImageColors
postMemberAttributesPostFileImageColorsDecoder =
    Json.Decode.succeed PostMemberAttributesPostFileImageColors
        |> Json.Decode.Pipeline.required "average_colors_of_corners" postMemberAttributesPostFileImageColorsAverageColorsOfCornersDecoder
        |> Json.Decode.Pipeline.required "dominant_color" Json.Decode.string
        |> Json.Decode.Pipeline.required "palette" (Json.Decode.list Json.Decode.string)
        |> Json.Decode.Pipeline.required "text_color" Json.Decode.string


postMemberAttributesPostFileImageColorsAverageColorsOfCornersDecoder : Json.Decode.Decoder PostMemberAttributesPostFileImageColorsAverageColorsOfCorners
postMemberAttributesPostFileImageColorsAverageColorsOfCornersDecoder =
    Json.Decode.succeed PostMemberAttributesPostFileImageColorsAverageColorsOfCorners
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
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list postMemberRelationshipsAccessRulesDataObjectDecoder)


postMemberRelationshipsAccessRulesDataObjectDecoder : Json.Decode.Decoder PostMemberRelationshipsAccessRulesDataObject
postMemberRelationshipsAccessRulesDataObjectDecoder =
    Json.Decode.succeed PostMemberRelationshipsAccessRulesDataObject
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postMemberRelationshipsCampaignDecoder : Json.Decode.Decoder PostMemberRelationshipsCampaign
postMemberRelationshipsCampaignDecoder =
    Json.Decode.succeed PostMemberRelationshipsCampaign
        |> Json.Decode.Pipeline.required "data" postMemberRelationshipsCampaignDataDecoder
        |> Json.Decode.Pipeline.required "links" postMemberRelationshipsCampaignLinksDecoder


postMemberRelationshipsCampaignDataDecoder : Json.Decode.Decoder PostMemberRelationshipsCampaignData
postMemberRelationshipsCampaignDataDecoder =
    Json.Decode.succeed PostMemberRelationshipsCampaignData
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postMemberRelationshipsCampaignLinksDecoder : Json.Decode.Decoder PostMemberRelationshipsCampaignLinks
postMemberRelationshipsCampaignLinksDecoder =
    Json.Decode.succeed PostMemberRelationshipsCampaignLinks
        |> Json.Decode.Pipeline.required "related" Json.Decode.string


postMemberRelationshipsImagesDecoder : Json.Decode.Decoder PostMemberRelationshipsImages
postMemberRelationshipsImagesDecoder =
    Json.Decode.succeed PostMemberRelationshipsImages
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list postMemberRelationshipsImagesDataObjectDecoder)


postMemberRelationshipsImagesDataObjectDecoder : Json.Decode.Decoder PostMemberRelationshipsImagesDataObject
postMemberRelationshipsImagesDataObjectDecoder =
    Json.Decode.succeed PostMemberRelationshipsImagesDataObject
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postMemberRelationshipsMediaDecoder : Json.Decode.Decoder PostMemberRelationshipsMedia
postMemberRelationshipsMediaDecoder =
    Json.Decode.succeed PostMemberRelationshipsMedia
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list postMemberRelationshipsMediaDataObjectDecoder)


postMemberRelationshipsMediaDataObjectDecoder : Json.Decode.Decoder PostMemberRelationshipsMediaDataObject
postMemberRelationshipsMediaDataObjectDecoder =
    Json.Decode.succeed PostMemberRelationshipsMediaDataObject
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postMemberRelationshipsUserDecoder : Json.Decode.Decoder PostMemberRelationshipsUser
postMemberRelationshipsUserDecoder =
    Json.Decode.succeed PostMemberRelationshipsUser
        |> Json.Decode.Pipeline.required "data" postMemberRelationshipsUserDataDecoder
        |> Json.Decode.Pipeline.required "links" postMemberRelationshipsUserLinksDecoder


postMemberRelationshipsUserDataDecoder : Json.Decode.Decoder PostMemberRelationshipsUserData
postMemberRelationshipsUserDataDecoder =
    Json.Decode.succeed PostMemberRelationshipsUserData
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


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
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list postEntityRelationshipsAccessRulesDataObjectDecoder)


postEntityRelationshipsAccessRulesDataObjectDecoder : Json.Decode.Decoder PostEntityRelationshipsAccessRulesDataObject
postEntityRelationshipsAccessRulesDataObjectDecoder =
    Json.Decode.succeed PostEntityRelationshipsAccessRulesDataObject
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postEntityRelationshipsCampaignDecoder : Json.Decode.Decoder PostEntityRelationshipsCampaign
postEntityRelationshipsCampaignDecoder =
    Json.Decode.succeed PostEntityRelationshipsCampaign
        |> Json.Decode.Pipeline.required "data" postEntityRelationshipsCampaignDataDecoder
        |> Json.Decode.Pipeline.required "links" postEntityRelationshipsCampaignLinksDecoder


postEntityRelationshipsCampaignDataDecoder : Json.Decode.Decoder PostEntityRelationshipsCampaignData
postEntityRelationshipsCampaignDataDecoder =
    Json.Decode.succeed PostEntityRelationshipsCampaignData
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postEntityRelationshipsCampaignLinksDecoder : Json.Decode.Decoder PostEntityRelationshipsCampaignLinks
postEntityRelationshipsCampaignLinksDecoder =
    Json.Decode.succeed PostEntityRelationshipsCampaignLinks
        |> Json.Decode.Pipeline.required "related" Json.Decode.string


postEntityRelationshipsUserDecoder : Json.Decode.Decoder PostEntityRelationshipsUser
postEntityRelationshipsUserDecoder =
    Json.Decode.succeed PostEntityRelationshipsUser
        |> Json.Decode.Pipeline.required "data" postEntityRelationshipsUserDataDecoder
        |> Json.Decode.Pipeline.required "links" postEntityRelationshipsUserLinksDecoder


postEntityRelationshipsUserDataDecoder : Json.Decode.Decoder PostEntityRelationshipsUserData
postEntityRelationshipsUserDataDecoder =
    Json.Decode.succeed PostEntityRelationshipsUserData
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


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
        |> Json.Decode.Pipeline.required "post_file" postThingAttributesPostFileDecoder
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


postThingAttributesPostFileDecoder : Json.Decode.Decoder PostThingAttributesPostFile
postThingAttributesPostFileDecoder =
    Json.Decode.succeed PostThingAttributesPostFile
        |> Json.Decode.Pipeline.required "default_thumbnail" hasUrlDecoder
        |> Json.Decode.Pipeline.required "duration" Json.Decode.int
        |> Json.Decode.Pipeline.required "full_content_duration" Json.Decode.int
        |> Json.Decode.Pipeline.required "media_id" Json.Decode.int
        |> Json.Decode.Pipeline.required "progress" progressDecoder
        |> Json.Decode.Pipeline.required "state" Json.Decode.string
        |> Json.Decode.Pipeline.required "url" Json.Decode.string


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
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list postThingRelationshipsAccessRulesDataObjectDecoder)


postThingRelationshipsAccessRulesDataObjectDecoder : Json.Decode.Decoder PostThingRelationshipsAccessRulesDataObject
postThingRelationshipsAccessRulesDataObjectDecoder =
    Json.Decode.succeed PostThingRelationshipsAccessRulesDataObject
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postThingRelationshipsAttachmentsMediaDecoder : Json.Decode.Decoder PostThingRelationshipsAttachmentsMedia
postThingRelationshipsAttachmentsMediaDecoder =
    Json.Decode.succeed PostThingRelationshipsAttachmentsMedia
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list postThingRelationshipsAttachmentsMediaDataObjectDecoder)


postThingRelationshipsAttachmentsMediaDataObjectDecoder : Json.Decode.Decoder PostThingRelationshipsAttachmentsMediaDataObject
postThingRelationshipsAttachmentsMediaDataObjectDecoder =
    Json.Decode.succeed PostThingRelationshipsAttachmentsMediaDataObject
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postThingRelationshipsAudioDecoder : Json.Decode.Decoder PostThingRelationshipsAudio
postThingRelationshipsAudioDecoder =
    Json.Decode.succeed PostThingRelationshipsAudio
        |> Json.Decode.Pipeline.required "data" postThingRelationshipsAudioDataDecoder
        |> Json.Decode.Pipeline.required "links" postThingRelationshipsAudioLinksDecoder


postThingRelationshipsAudioDataDecoder : Json.Decode.Decoder PostThingRelationshipsAudioData
postThingRelationshipsAudioDataDecoder =
    Json.Decode.succeed PostThingRelationshipsAudioData
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postThingRelationshipsAudioLinksDecoder : Json.Decode.Decoder PostThingRelationshipsAudioLinks
postThingRelationshipsAudioLinksDecoder =
    Json.Decode.succeed PostThingRelationshipsAudioLinks
        |> Json.Decode.Pipeline.required "related" Json.Decode.string


postThingRelationshipsCampaignDecoder : Json.Decode.Decoder PostThingRelationshipsCampaign
postThingRelationshipsCampaignDecoder =
    Json.Decode.succeed PostThingRelationshipsCampaign
        |> Json.Decode.Pipeline.required "data" postThingRelationshipsCampaignDataDecoder
        |> Json.Decode.Pipeline.required "links" postThingRelationshipsCampaignLinksDecoder


postThingRelationshipsCampaignDataDecoder : Json.Decode.Decoder PostThingRelationshipsCampaignData
postThingRelationshipsCampaignDataDecoder =
    Json.Decode.succeed PostThingRelationshipsCampaignData
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postThingRelationshipsCampaignLinksDecoder : Json.Decode.Decoder PostThingRelationshipsCampaignLinks
postThingRelationshipsCampaignLinksDecoder =
    Json.Decode.succeed PostThingRelationshipsCampaignLinks
        |> Json.Decode.Pipeline.required "related" Json.Decode.string


postThingRelationshipsImagesDecoder : Json.Decode.Decoder PostThingRelationshipsImages
postThingRelationshipsImagesDecoder =
    Json.Decode.succeed PostThingRelationshipsImages
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list postThingRelationshipsImagesDataObjectDecoder)


postThingRelationshipsImagesDataObjectDecoder : Json.Decode.Decoder PostThingRelationshipsImagesDataObject
postThingRelationshipsImagesDataObjectDecoder =
    Json.Decode.succeed PostThingRelationshipsImagesDataObject
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postThingRelationshipsMediaDecoder : Json.Decode.Decoder PostThingRelationshipsMedia
postThingRelationshipsMediaDecoder =
    Json.Decode.succeed PostThingRelationshipsMedia
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list postThingRelationshipsMediaDataObjectDecoder)


postThingRelationshipsMediaDataObjectDecoder : Json.Decode.Decoder PostThingRelationshipsMediaDataObject
postThingRelationshipsMediaDataObjectDecoder =
    Json.Decode.succeed PostThingRelationshipsMediaDataObject
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postThingRelationshipsUserDecoder : Json.Decode.Decoder PostThingRelationshipsUser
postThingRelationshipsUserDecoder =
    Json.Decode.succeed PostThingRelationshipsUser
        |> Json.Decode.Pipeline.required "data" postThingRelationshipsUserDataDecoder
        |> Json.Decode.Pipeline.required "links" postThingRelationshipsUserLinksDecoder


postThingRelationshipsUserDataDecoder : Json.Decode.Decoder PostThingRelationshipsUserData
postThingRelationshipsUserDataDecoder =
    Json.Decode.succeed PostThingRelationshipsUserData
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


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
        |> Json.Decode.Pipeline.required "image_colors" postInstanceAttributesPostFileImageColorsDecoder
        |> Json.Decode.Pipeline.required "media_id" Json.Decode.int
        |> Json.Decode.Pipeline.required "state" Json.Decode.string
        |> Json.Decode.Pipeline.required "url" Json.Decode.string
        |> Json.Decode.Pipeline.required "width" Json.Decode.int


postInstanceAttributesPostFileImageColorsDecoder : Json.Decode.Decoder PostInstanceAttributesPostFileImageColors
postInstanceAttributesPostFileImageColorsDecoder =
    Json.Decode.succeed PostInstanceAttributesPostFileImageColors
        |> Json.Decode.Pipeline.required "average_colors_of_corners" postInstanceAttributesPostFileImageColorsAverageColorsOfCornersDecoder
        |> Json.Decode.Pipeline.required "dominant_color" Json.Decode.string
        |> Json.Decode.Pipeline.required "palette" (Json.Decode.list Json.Decode.string)
        |> Json.Decode.Pipeline.required "text_color" Json.Decode.string


postInstanceAttributesPostFileImageColorsAverageColorsOfCornersDecoder : Json.Decode.Decoder PostInstanceAttributesPostFileImageColorsAverageColorsOfCorners
postInstanceAttributesPostFileImageColorsAverageColorsOfCornersDecoder =
    Json.Decode.succeed PostInstanceAttributesPostFileImageColorsAverageColorsOfCorners
        |> Json.Decode.Pipeline.required "bottom_left" Json.Decode.string
        |> Json.Decode.Pipeline.required "bottom_right" Json.Decode.string
        |> Json.Decode.Pipeline.required "top_left" Json.Decode.string
        |> Json.Decode.Pipeline.required "top_right" Json.Decode.string


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
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list postInstanceRelationshipsAccessRulesDataObjectDecoder)


postInstanceRelationshipsAccessRulesDataObjectDecoder : Json.Decode.Decoder PostInstanceRelationshipsAccessRulesDataObject
postInstanceRelationshipsAccessRulesDataObjectDecoder =
    Json.Decode.succeed PostInstanceRelationshipsAccessRulesDataObject
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postInstanceRelationshipsCampaignDecoder : Json.Decode.Decoder PostInstanceRelationshipsCampaign
postInstanceRelationshipsCampaignDecoder =
    Json.Decode.succeed PostInstanceRelationshipsCampaign
        |> Json.Decode.Pipeline.required "data" postInstanceRelationshipsCampaignDataDecoder
        |> Json.Decode.Pipeline.required "links" postInstanceRelationshipsCampaignLinksDecoder


postInstanceRelationshipsCampaignDataDecoder : Json.Decode.Decoder PostInstanceRelationshipsCampaignData
postInstanceRelationshipsCampaignDataDecoder =
    Json.Decode.succeed PostInstanceRelationshipsCampaignData
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postInstanceRelationshipsCampaignLinksDecoder : Json.Decode.Decoder PostInstanceRelationshipsCampaignLinks
postInstanceRelationshipsCampaignLinksDecoder =
    Json.Decode.succeed PostInstanceRelationshipsCampaignLinks
        |> Json.Decode.Pipeline.required "related" Json.Decode.string


postInstanceRelationshipsImagesDecoder : Json.Decode.Decoder PostInstanceRelationshipsImages
postInstanceRelationshipsImagesDecoder =
    Json.Decode.succeed PostInstanceRelationshipsImages
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list postInstanceRelationshipsImagesDataObjectDecoder)


postInstanceRelationshipsImagesDataObjectDecoder : Json.Decode.Decoder PostInstanceRelationshipsImagesDataObject
postInstanceRelationshipsImagesDataObjectDecoder =
    Json.Decode.succeed PostInstanceRelationshipsImagesDataObject
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postInstanceRelationshipsMediaDecoder : Json.Decode.Decoder PostInstanceRelationshipsMedia
postInstanceRelationshipsMediaDecoder =
    Json.Decode.succeed PostInstanceRelationshipsMedia
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list postInstanceRelationshipsMediaDataObjectDecoder)


postInstanceRelationshipsMediaDataObjectDecoder : Json.Decode.Decoder PostInstanceRelationshipsMediaDataObject
postInstanceRelationshipsMediaDataObjectDecoder =
    Json.Decode.succeed PostInstanceRelationshipsMediaDataObject
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postInstanceRelationshipsUserDecoder : Json.Decode.Decoder PostInstanceRelationshipsUser
postInstanceRelationshipsUserDecoder =
    Json.Decode.succeed PostInstanceRelationshipsUser
        |> Json.Decode.Pipeline.required "data" postInstanceRelationshipsUserDataDecoder
        |> Json.Decode.Pipeline.required "links" postInstanceRelationshipsUserLinksDecoder


postInstanceRelationshipsUserDataDecoder : Json.Decode.Decoder PostInstanceRelationshipsUserData
postInstanceRelationshipsUserDataDecoder =
    Json.Decode.succeed PostInstanceRelationshipsUserData
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


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
        |> Json.Decode.Pipeline.required "image_colors" postConstituentAttributesPostFileImageColorsDecoder
        |> Json.Decode.Pipeline.required "media_id" Json.Decode.int
        |> Json.Decode.Pipeline.required "state" Json.Decode.string
        |> Json.Decode.Pipeline.required "url" Json.Decode.string
        |> Json.Decode.Pipeline.required "width" Json.Decode.int


postConstituentAttributesPostFileImageColorsDecoder : Json.Decode.Decoder PostConstituentAttributesPostFileImageColors
postConstituentAttributesPostFileImageColorsDecoder =
    Json.Decode.succeed PostConstituentAttributesPostFileImageColors
        |> Json.Decode.Pipeline.required "average_colors_of_corners" postConstituentAttributesPostFileImageColorsAverageColorsOfCornersDecoder
        |> Json.Decode.Pipeline.required "dominant_color" Json.Decode.string
        |> Json.Decode.Pipeline.required "palette" (Json.Decode.list Json.Decode.string)
        |> Json.Decode.Pipeline.required "text_color" Json.Decode.string


postConstituentAttributesPostFileImageColorsAverageColorsOfCornersDecoder : Json.Decode.Decoder PostConstituentAttributesPostFileImageColorsAverageColorsOfCorners
postConstituentAttributesPostFileImageColorsAverageColorsOfCornersDecoder =
    Json.Decode.succeed PostConstituentAttributesPostFileImageColorsAverageColorsOfCorners
        |> Json.Decode.Pipeline.required "bottom_left" Json.Decode.string
        |> Json.Decode.Pipeline.required "bottom_right" Json.Decode.string
        |> Json.Decode.Pipeline.required "top_left" Json.Decode.string
        |> Json.Decode.Pipeline.required "top_right" Json.Decode.string


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
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list postConstituentRelationshipsAccessRulesDataObjectDecoder)


postConstituentRelationshipsAccessRulesDataObjectDecoder : Json.Decode.Decoder PostConstituentRelationshipsAccessRulesDataObject
postConstituentRelationshipsAccessRulesDataObjectDecoder =
    Json.Decode.succeed PostConstituentRelationshipsAccessRulesDataObject
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postConstituentRelationshipsCampaignDecoder : Json.Decode.Decoder PostConstituentRelationshipsCampaign
postConstituentRelationshipsCampaignDecoder =
    Json.Decode.succeed PostConstituentRelationshipsCampaign
        |> Json.Decode.Pipeline.required "data" postConstituentRelationshipsCampaignDataDecoder
        |> Json.Decode.Pipeline.required "links" postConstituentRelationshipsCampaignLinksDecoder


postConstituentRelationshipsCampaignDataDecoder : Json.Decode.Decoder PostConstituentRelationshipsCampaignData
postConstituentRelationshipsCampaignDataDecoder =
    Json.Decode.succeed PostConstituentRelationshipsCampaignData
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postConstituentRelationshipsCampaignLinksDecoder : Json.Decode.Decoder PostConstituentRelationshipsCampaignLinks
postConstituentRelationshipsCampaignLinksDecoder =
    Json.Decode.succeed PostConstituentRelationshipsCampaignLinks
        |> Json.Decode.Pipeline.required "related" Json.Decode.string


postConstituentRelationshipsImagesDecoder : Json.Decode.Decoder PostConstituentRelationshipsImages
postConstituentRelationshipsImagesDecoder =
    Json.Decode.succeed PostConstituentRelationshipsImages
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list postConstituentRelationshipsImagesDataObjectDecoder)


postConstituentRelationshipsImagesDataObjectDecoder : Json.Decode.Decoder PostConstituentRelationshipsImagesDataObject
postConstituentRelationshipsImagesDataObjectDecoder =
    Json.Decode.succeed PostConstituentRelationshipsImagesDataObject
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postConstituentRelationshipsMediaDecoder : Json.Decode.Decoder PostConstituentRelationshipsMedia
postConstituentRelationshipsMediaDecoder =
    Json.Decode.succeed PostConstituentRelationshipsMedia
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list postConstituentRelationshipsMediaDataObjectDecoder)


postConstituentRelationshipsMediaDataObjectDecoder : Json.Decode.Decoder PostConstituentRelationshipsMediaDataObject
postConstituentRelationshipsMediaDataObjectDecoder =
    Json.Decode.succeed PostConstituentRelationshipsMediaDataObject
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postConstituentRelationshipsUserDecoder : Json.Decode.Decoder PostConstituentRelationshipsUser
postConstituentRelationshipsUserDecoder =
    Json.Decode.succeed PostConstituentRelationshipsUser
        |> Json.Decode.Pipeline.required "data" postConstituentRelationshipsUserDataDecoder
        |> Json.Decode.Pipeline.required "links" postConstituentRelationshipsUserLinksDecoder


postConstituentRelationshipsUserDataDecoder : Json.Decode.Decoder PostConstituentRelationshipsUserData
postConstituentRelationshipsUserDataDecoder =
    Json.Decode.succeed PostConstituentRelationshipsUserData
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


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
        |> Json.Decode.Pipeline.required "image_colors" postSpecimenAttributesPostFileImageColorsDecoder
        |> Json.Decode.Pipeline.required "media_id" Json.Decode.int
        |> Json.Decode.Pipeline.required "state" Json.Decode.string
        |> Json.Decode.Pipeline.required "url" Json.Decode.string
        |> Json.Decode.Pipeline.required "width" Json.Decode.int


postSpecimenAttributesPostFileImageColorsDecoder : Json.Decode.Decoder PostSpecimenAttributesPostFileImageColors
postSpecimenAttributesPostFileImageColorsDecoder =
    Json.Decode.succeed PostSpecimenAttributesPostFileImageColors
        |> Json.Decode.Pipeline.required "average_colors_of_corners" postSpecimenAttributesPostFileImageColorsAverageColorsOfCornersDecoder
        |> Json.Decode.Pipeline.required "dominant_color" Json.Decode.string
        |> Json.Decode.Pipeline.required "palette" (Json.Decode.list Json.Decode.string)
        |> Json.Decode.Pipeline.required "text_color" Json.Decode.string


postSpecimenAttributesPostFileImageColorsAverageColorsOfCornersDecoder : Json.Decode.Decoder PostSpecimenAttributesPostFileImageColorsAverageColorsOfCorners
postSpecimenAttributesPostFileImageColorsAverageColorsOfCornersDecoder =
    Json.Decode.succeed PostSpecimenAttributesPostFileImageColorsAverageColorsOfCorners
        |> Json.Decode.Pipeline.required "bottom_left" Json.Decode.string
        |> Json.Decode.Pipeline.required "bottom_right" Json.Decode.string
        |> Json.Decode.Pipeline.required "top_left" Json.Decode.string
        |> Json.Decode.Pipeline.required "top_right" Json.Decode.string


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
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list postSpecimenRelationshipsAccessRulesDataObjectDecoder)


postSpecimenRelationshipsAccessRulesDataObjectDecoder : Json.Decode.Decoder PostSpecimenRelationshipsAccessRulesDataObject
postSpecimenRelationshipsAccessRulesDataObjectDecoder =
    Json.Decode.succeed PostSpecimenRelationshipsAccessRulesDataObject
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postSpecimenRelationshipsCampaignDecoder : Json.Decode.Decoder PostSpecimenRelationshipsCampaign
postSpecimenRelationshipsCampaignDecoder =
    Json.Decode.succeed PostSpecimenRelationshipsCampaign
        |> Json.Decode.Pipeline.required "data" postSpecimenRelationshipsCampaignDataDecoder
        |> Json.Decode.Pipeline.required "links" postSpecimenRelationshipsCampaignLinksDecoder


postSpecimenRelationshipsCampaignDataDecoder : Json.Decode.Decoder PostSpecimenRelationshipsCampaignData
postSpecimenRelationshipsCampaignDataDecoder =
    Json.Decode.succeed PostSpecimenRelationshipsCampaignData
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postSpecimenRelationshipsCampaignLinksDecoder : Json.Decode.Decoder PostSpecimenRelationshipsCampaignLinks
postSpecimenRelationshipsCampaignLinksDecoder =
    Json.Decode.succeed PostSpecimenRelationshipsCampaignLinks
        |> Json.Decode.Pipeline.required "related" Json.Decode.string


postSpecimenRelationshipsImagesDecoder : Json.Decode.Decoder PostSpecimenRelationshipsImages
postSpecimenRelationshipsImagesDecoder =
    Json.Decode.succeed PostSpecimenRelationshipsImages
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list postSpecimenRelationshipsImagesDataObjectDecoder)


postSpecimenRelationshipsImagesDataObjectDecoder : Json.Decode.Decoder PostSpecimenRelationshipsImagesDataObject
postSpecimenRelationshipsImagesDataObjectDecoder =
    Json.Decode.succeed PostSpecimenRelationshipsImagesDataObject
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postSpecimenRelationshipsMediaDecoder : Json.Decode.Decoder PostSpecimenRelationshipsMedia
postSpecimenRelationshipsMediaDecoder =
    Json.Decode.succeed PostSpecimenRelationshipsMedia
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list postSpecimenRelationshipsMediaDataObjectDecoder)


postSpecimenRelationshipsMediaDataObjectDecoder : Json.Decode.Decoder PostSpecimenRelationshipsMediaDataObject
postSpecimenRelationshipsMediaDataObjectDecoder =
    Json.Decode.succeed PostSpecimenRelationshipsMediaDataObject
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postSpecimenRelationshipsUserDecoder : Json.Decode.Decoder PostSpecimenRelationshipsUser
postSpecimenRelationshipsUserDecoder =
    Json.Decode.succeed PostSpecimenRelationshipsUser
        |> Json.Decode.Pipeline.required "data" postSpecimenRelationshipsUserDataDecoder
        |> Json.Decode.Pipeline.required "links" postSpecimenRelationshipsUserLinksDecoder


postSpecimenRelationshipsUserDataDecoder : Json.Decode.Decoder PostSpecimenRelationshipsUserData
postSpecimenRelationshipsUserDataDecoder =
    Json.Decode.succeed PostSpecimenRelationshipsUserData
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postSpecimenRelationshipsUserLinksDecoder : Json.Decode.Decoder PostSpecimenRelationshipsUserLinks
postSpecimenRelationshipsUserLinksDecoder =
    Json.Decode.succeed PostSpecimenRelationshipsUserLinks
        |> Json.Decode.Pipeline.required "related" Json.Decode.string


postSpecimenRelationshipsUserDefinedTagsDecoder : Json.Decode.Decoder PostSpecimenRelationshipsUserDefinedTags
postSpecimenRelationshipsUserDefinedTagsDecoder =
    Json.Decode.succeed PostSpecimenRelationshipsUserDefinedTags
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list postSpecimenRelationshipsUserDefinedTagsDataObjectDecoder)


postSpecimenRelationshipsUserDefinedTagsDataObjectDecoder : Json.Decode.Decoder PostSpecimenRelationshipsUserDefinedTagsDataObject
postSpecimenRelationshipsUserDefinedTagsDataObjectDecoder =
    Json.Decode.succeed PostSpecimenRelationshipsUserDefinedTagsDataObject
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


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
        |> Json.Decode.Pipeline.required "post_file" postGadgetAttributesPostFileDecoder
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


postGadgetAttributesPostFileDecoder : Json.Decode.Decoder PostGadgetAttributesPostFile
postGadgetAttributesPostFileDecoder =
    Json.Decode.succeed PostGadgetAttributesPostFile
        |> Json.Decode.Pipeline.required "default_thumbnail" hasUrlDecoder
        |> Json.Decode.Pipeline.required "duration" Json.Decode.int
        |> Json.Decode.Pipeline.required "full_content_duration" Json.Decode.int
        |> Json.Decode.Pipeline.required "media_id" Json.Decode.int
        |> Json.Decode.Pipeline.required "progress" progressDecoder
        |> Json.Decode.Pipeline.required "state" Json.Decode.string
        |> Json.Decode.Pipeline.required "url" Json.Decode.string


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
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list postGadgetRelationshipsAccessRulesDataObjectDecoder)


postGadgetRelationshipsAccessRulesDataObjectDecoder : Json.Decode.Decoder PostGadgetRelationshipsAccessRulesDataObject
postGadgetRelationshipsAccessRulesDataObjectDecoder =
    Json.Decode.succeed PostGadgetRelationshipsAccessRulesDataObject
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postGadgetRelationshipsAudioDecoder : Json.Decode.Decoder PostGadgetRelationshipsAudio
postGadgetRelationshipsAudioDecoder =
    Json.Decode.succeed PostGadgetRelationshipsAudio
        |> Json.Decode.Pipeline.required "data" postGadgetRelationshipsAudioDataDecoder
        |> Json.Decode.Pipeline.required "links" postGadgetRelationshipsAudioLinksDecoder


postGadgetRelationshipsAudioDataDecoder : Json.Decode.Decoder PostGadgetRelationshipsAudioData
postGadgetRelationshipsAudioDataDecoder =
    Json.Decode.succeed PostGadgetRelationshipsAudioData
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postGadgetRelationshipsAudioLinksDecoder : Json.Decode.Decoder PostGadgetRelationshipsAudioLinks
postGadgetRelationshipsAudioLinksDecoder =
    Json.Decode.succeed PostGadgetRelationshipsAudioLinks
        |> Json.Decode.Pipeline.required "related" Json.Decode.string


postGadgetRelationshipsCampaignDecoder : Json.Decode.Decoder PostGadgetRelationshipsCampaign
postGadgetRelationshipsCampaignDecoder =
    Json.Decode.succeed PostGadgetRelationshipsCampaign
        |> Json.Decode.Pipeline.required "data" postGadgetRelationshipsCampaignDataDecoder
        |> Json.Decode.Pipeline.required "links" postGadgetRelationshipsCampaignLinksDecoder


postGadgetRelationshipsCampaignDataDecoder : Json.Decode.Decoder PostGadgetRelationshipsCampaignData
postGadgetRelationshipsCampaignDataDecoder =
    Json.Decode.succeed PostGadgetRelationshipsCampaignData
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postGadgetRelationshipsCampaignLinksDecoder : Json.Decode.Decoder PostGadgetRelationshipsCampaignLinks
postGadgetRelationshipsCampaignLinksDecoder =
    Json.Decode.succeed PostGadgetRelationshipsCampaignLinks
        |> Json.Decode.Pipeline.required "related" Json.Decode.string


postGadgetRelationshipsMediaDecoder : Json.Decode.Decoder PostGadgetRelationshipsMedia
postGadgetRelationshipsMediaDecoder =
    Json.Decode.succeed PostGadgetRelationshipsMedia
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list postGadgetRelationshipsMediaDataObjectDecoder)


postGadgetRelationshipsMediaDataObjectDecoder : Json.Decode.Decoder PostGadgetRelationshipsMediaDataObject
postGadgetRelationshipsMediaDataObjectDecoder =
    Json.Decode.succeed PostGadgetRelationshipsMediaDataObject
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postGadgetRelationshipsUserDecoder : Json.Decode.Decoder PostGadgetRelationshipsUser
postGadgetRelationshipsUserDecoder =
    Json.Decode.succeed PostGadgetRelationshipsUser
        |> Json.Decode.Pipeline.required "data" postGadgetRelationshipsUserDataDecoder
        |> Json.Decode.Pipeline.required "links" postGadgetRelationshipsUserLinksDecoder


postGadgetRelationshipsUserDataDecoder : Json.Decode.Decoder PostGadgetRelationshipsUserData
postGadgetRelationshipsUserDataDecoder =
    Json.Decode.succeed PostGadgetRelationshipsUserData
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postGadgetRelationshipsUserLinksDecoder : Json.Decode.Decoder PostGadgetRelationshipsUserLinks
postGadgetRelationshipsUserLinksDecoder =
    Json.Decode.succeed PostGadgetRelationshipsUserLinks
        |> Json.Decode.Pipeline.required "related" Json.Decode.string


postGadgetRelationshipsUserDefinedTagsDecoder : Json.Decode.Decoder PostGadgetRelationshipsUserDefinedTags
postGadgetRelationshipsUserDefinedTagsDecoder =
    Json.Decode.succeed PostGadgetRelationshipsUserDefinedTags
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list postGadgetRelationshipsUserDefinedTagsDataObjectDecoder)


postGadgetRelationshipsUserDefinedTagsDataObjectDecoder : Json.Decode.Decoder PostGadgetRelationshipsUserDefinedTagsDataObject
postGadgetRelationshipsUserDefinedTagsDataObjectDecoder =
    Json.Decode.succeed PostGadgetRelationshipsUserDefinedTagsDataObject
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


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
        |> Json.Decode.Pipeline.required "image_colors" postWidgetAttributesPostFileImageColorsDecoder
        |> Json.Decode.Pipeline.required "media_id" Json.Decode.int
        |> Json.Decode.Pipeline.required "state" Json.Decode.string
        |> Json.Decode.Pipeline.required "url" Json.Decode.string
        |> Json.Decode.Pipeline.required "width" Json.Decode.int


postWidgetAttributesPostFileImageColorsDecoder : Json.Decode.Decoder PostWidgetAttributesPostFileImageColors
postWidgetAttributesPostFileImageColorsDecoder =
    Json.Decode.succeed PostWidgetAttributesPostFileImageColors
        |> Json.Decode.Pipeline.required "average_colors_of_corners" postWidgetAttributesPostFileImageColorsAverageColorsOfCornersDecoder
        |> Json.Decode.Pipeline.required "dominant_color" Json.Decode.string
        |> Json.Decode.Pipeline.required "palette" (Json.Decode.list Json.Decode.string)
        |> Json.Decode.Pipeline.required "text_color" Json.Decode.string


postWidgetAttributesPostFileImageColorsAverageColorsOfCornersDecoder : Json.Decode.Decoder PostWidgetAttributesPostFileImageColorsAverageColorsOfCorners
postWidgetAttributesPostFileImageColorsAverageColorsOfCornersDecoder =
    Json.Decode.succeed PostWidgetAttributesPostFileImageColorsAverageColorsOfCorners
        |> Json.Decode.Pipeline.required "bottom_left" Json.Decode.string
        |> Json.Decode.Pipeline.required "bottom_right" Json.Decode.string
        |> Json.Decode.Pipeline.required "top_left" Json.Decode.string
        |> Json.Decode.Pipeline.required "top_right" Json.Decode.string


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
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list postWidgetRelationshipsAccessRulesDataObjectDecoder)


postWidgetRelationshipsAccessRulesDataObjectDecoder : Json.Decode.Decoder PostWidgetRelationshipsAccessRulesDataObject
postWidgetRelationshipsAccessRulesDataObjectDecoder =
    Json.Decode.succeed PostWidgetRelationshipsAccessRulesDataObject
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postWidgetRelationshipsCampaignDecoder : Json.Decode.Decoder PostWidgetRelationshipsCampaign
postWidgetRelationshipsCampaignDecoder =
    Json.Decode.succeed PostWidgetRelationshipsCampaign
        |> Json.Decode.Pipeline.required "data" postWidgetRelationshipsCampaignDataDecoder
        |> Json.Decode.Pipeline.required "links" postWidgetRelationshipsCampaignLinksDecoder


postWidgetRelationshipsCampaignDataDecoder : Json.Decode.Decoder PostWidgetRelationshipsCampaignData
postWidgetRelationshipsCampaignDataDecoder =
    Json.Decode.succeed PostWidgetRelationshipsCampaignData
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postWidgetRelationshipsCampaignLinksDecoder : Json.Decode.Decoder PostWidgetRelationshipsCampaignLinks
postWidgetRelationshipsCampaignLinksDecoder =
    Json.Decode.succeed PostWidgetRelationshipsCampaignLinks
        |> Json.Decode.Pipeline.required "related" Json.Decode.string


postWidgetRelationshipsImagesDecoder : Json.Decode.Decoder PostWidgetRelationshipsImages
postWidgetRelationshipsImagesDecoder =
    Json.Decode.succeed PostWidgetRelationshipsImages
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list postWidgetRelationshipsImagesDataObjectDecoder)


postWidgetRelationshipsImagesDataObjectDecoder : Json.Decode.Decoder PostWidgetRelationshipsImagesDataObject
postWidgetRelationshipsImagesDataObjectDecoder =
    Json.Decode.succeed PostWidgetRelationshipsImagesDataObject
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postWidgetRelationshipsMediaDecoder : Json.Decode.Decoder PostWidgetRelationshipsMedia
postWidgetRelationshipsMediaDecoder =
    Json.Decode.succeed PostWidgetRelationshipsMedia
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list postWidgetRelationshipsMediaDataObjectDecoder)


postWidgetRelationshipsMediaDataObjectDecoder : Json.Decode.Decoder PostWidgetRelationshipsMediaDataObject
postWidgetRelationshipsMediaDataObjectDecoder =
    Json.Decode.succeed PostWidgetRelationshipsMediaDataObject
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postWidgetRelationshipsUserDecoder : Json.Decode.Decoder PostWidgetRelationshipsUser
postWidgetRelationshipsUserDecoder =
    Json.Decode.succeed PostWidgetRelationshipsUser
        |> Json.Decode.Pipeline.required "data" postWidgetRelationshipsUserDataDecoder
        |> Json.Decode.Pipeline.required "links" postWidgetRelationshipsUserLinksDecoder


postWidgetRelationshipsUserDataDecoder : Json.Decode.Decoder PostWidgetRelationshipsUserData
postWidgetRelationshipsUserDataDecoder =
    Json.Decode.succeed PostWidgetRelationshipsUserData
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postWidgetRelationshipsUserLinksDecoder : Json.Decode.Decoder PostWidgetRelationshipsUserLinks
postWidgetRelationshipsUserLinksDecoder =
    Json.Decode.succeed PostWidgetRelationshipsUserLinks
        |> Json.Decode.Pipeline.required "related" Json.Decode.string


postWidgetRelationshipsUserDefinedTagsDecoder : Json.Decode.Decoder PostWidgetRelationshipsUserDefinedTags
postWidgetRelationshipsUserDefinedTagsDecoder =
    Json.Decode.succeed PostWidgetRelationshipsUserDefinedTags
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list postWidgetRelationshipsUserDefinedTagsDataObjectDecoder)


postWidgetRelationshipsUserDefinedTagsDataObjectDecoder : Json.Decode.Decoder PostWidgetRelationshipsUserDefinedTagsDataObject
postWidgetRelationshipsUserDefinedTagsDataObjectDecoder =
    Json.Decode.succeed PostWidgetRelationshipsUserDefinedTagsDataObject
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


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
        |> Json.Decode.Pipeline.required "post_file" postGizmoAttributesPostFileDecoder
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


postGizmoAttributesPostFileDecoder : Json.Decode.Decoder PostGizmoAttributesPostFile
postGizmoAttributesPostFileDecoder =
    Json.Decode.succeed PostGizmoAttributesPostFile
        |> Json.Decode.Pipeline.required "default_thumbnail" hasUrlDecoder
        |> Json.Decode.Pipeline.required "duration" Json.Decode.int
        |> Json.Decode.Pipeline.required "full_content_duration" Json.Decode.int
        |> Json.Decode.Pipeline.required "media_id" Json.Decode.int
        |> Json.Decode.Pipeline.required "progress" progressDecoder
        |> Json.Decode.Pipeline.required "state" Json.Decode.string
        |> Json.Decode.Pipeline.required "url" Json.Decode.string


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
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list postGizmoRelationshipsAccessRulesDataObjectDecoder)


postGizmoRelationshipsAccessRulesDataObjectDecoder : Json.Decode.Decoder PostGizmoRelationshipsAccessRulesDataObject
postGizmoRelationshipsAccessRulesDataObjectDecoder =
    Json.Decode.succeed PostGizmoRelationshipsAccessRulesDataObject
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postGizmoRelationshipsAudioDecoder : Json.Decode.Decoder PostGizmoRelationshipsAudio
postGizmoRelationshipsAudioDecoder =
    Json.Decode.succeed PostGizmoRelationshipsAudio
        |> Json.Decode.Pipeline.required "data" postGizmoRelationshipsAudioDataDecoder
        |> Json.Decode.Pipeline.required "links" postGizmoRelationshipsAudioLinksDecoder


postGizmoRelationshipsAudioDataDecoder : Json.Decode.Decoder PostGizmoRelationshipsAudioData
postGizmoRelationshipsAudioDataDecoder =
    Json.Decode.succeed PostGizmoRelationshipsAudioData
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postGizmoRelationshipsAudioLinksDecoder : Json.Decode.Decoder PostGizmoRelationshipsAudioLinks
postGizmoRelationshipsAudioLinksDecoder =
    Json.Decode.succeed PostGizmoRelationshipsAudioLinks
        |> Json.Decode.Pipeline.required "related" Json.Decode.string


postGizmoRelationshipsAudioPreviewDecoder : Json.Decode.Decoder PostGizmoRelationshipsAudioPreview
postGizmoRelationshipsAudioPreviewDecoder =
    Json.Decode.succeed PostGizmoRelationshipsAudioPreview
        |> Json.Decode.Pipeline.required "data" postGizmoRelationshipsAudioPreviewDataDecoder
        |> Json.Decode.Pipeline.required "links" postGizmoRelationshipsAudioPreviewLinksDecoder


postGizmoRelationshipsAudioPreviewDataDecoder : Json.Decode.Decoder PostGizmoRelationshipsAudioPreviewData
postGizmoRelationshipsAudioPreviewDataDecoder =
    Json.Decode.succeed PostGizmoRelationshipsAudioPreviewData
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postGizmoRelationshipsAudioPreviewLinksDecoder : Json.Decode.Decoder PostGizmoRelationshipsAudioPreviewLinks
postGizmoRelationshipsAudioPreviewLinksDecoder =
    Json.Decode.succeed PostGizmoRelationshipsAudioPreviewLinks
        |> Json.Decode.Pipeline.required "related" Json.Decode.string


postGizmoRelationshipsCampaignDecoder : Json.Decode.Decoder PostGizmoRelationshipsCampaign
postGizmoRelationshipsCampaignDecoder =
    Json.Decode.succeed PostGizmoRelationshipsCampaign
        |> Json.Decode.Pipeline.required "data" postGizmoRelationshipsCampaignDataDecoder
        |> Json.Decode.Pipeline.required "links" postGizmoRelationshipsCampaignLinksDecoder


postGizmoRelationshipsCampaignDataDecoder : Json.Decode.Decoder PostGizmoRelationshipsCampaignData
postGizmoRelationshipsCampaignDataDecoder =
    Json.Decode.succeed PostGizmoRelationshipsCampaignData
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postGizmoRelationshipsCampaignLinksDecoder : Json.Decode.Decoder PostGizmoRelationshipsCampaignLinks
postGizmoRelationshipsCampaignLinksDecoder =
    Json.Decode.succeed PostGizmoRelationshipsCampaignLinks
        |> Json.Decode.Pipeline.required "related" Json.Decode.string


postGizmoRelationshipsImagesDecoder : Json.Decode.Decoder PostGizmoRelationshipsImages
postGizmoRelationshipsImagesDecoder =
    Json.Decode.succeed PostGizmoRelationshipsImages
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list postGizmoRelationshipsImagesDataObjectDecoder)


postGizmoRelationshipsImagesDataObjectDecoder : Json.Decode.Decoder PostGizmoRelationshipsImagesDataObject
postGizmoRelationshipsImagesDataObjectDecoder =
    Json.Decode.succeed PostGizmoRelationshipsImagesDataObject
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postGizmoRelationshipsMediaDecoder : Json.Decode.Decoder PostGizmoRelationshipsMedia
postGizmoRelationshipsMediaDecoder =
    Json.Decode.succeed PostGizmoRelationshipsMedia
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list postGizmoRelationshipsMediaDataObjectDecoder)


postGizmoRelationshipsMediaDataObjectDecoder : Json.Decode.Decoder PostGizmoRelationshipsMediaDataObject
postGizmoRelationshipsMediaDataObjectDecoder =
    Json.Decode.succeed PostGizmoRelationshipsMediaDataObject
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postGizmoRelationshipsUserDecoder : Json.Decode.Decoder PostGizmoRelationshipsUser
postGizmoRelationshipsUserDecoder =
    Json.Decode.succeed PostGizmoRelationshipsUser
        |> Json.Decode.Pipeline.required "data" postGizmoRelationshipsUserDataDecoder
        |> Json.Decode.Pipeline.required "links" postGizmoRelationshipsUserLinksDecoder


postGizmoRelationshipsUserDataDecoder : Json.Decode.Decoder PostGizmoRelationshipsUserData
postGizmoRelationshipsUserDataDecoder =
    Json.Decode.succeed PostGizmoRelationshipsUserData
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


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
        |> Json.Decode.Pipeline.required "post_file" postPartAttributesPostFileDecoder
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


postPartAttributesPostFileDecoder : Json.Decode.Decoder PostPartAttributesPostFile
postPartAttributesPostFileDecoder =
    Json.Decode.succeed PostPartAttributesPostFile
        |> Json.Decode.Pipeline.required "default_thumbnail" hasUrlDecoder
        |> Json.Decode.Pipeline.required "duration" Json.Decode.int
        |> Json.Decode.Pipeline.required "full_content_duration" Json.Decode.int
        |> Json.Decode.Pipeline.required "media_id" Json.Decode.int
        |> Json.Decode.Pipeline.required "progress" progressDecoder
        |> Json.Decode.Pipeline.required "state" Json.Decode.string
        |> Json.Decode.Pipeline.required "url" Json.Decode.string


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
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list postPartRelationshipsAccessRulesDataObjectDecoder)


postPartRelationshipsAccessRulesDataObjectDecoder : Json.Decode.Decoder PostPartRelationshipsAccessRulesDataObject
postPartRelationshipsAccessRulesDataObjectDecoder =
    Json.Decode.succeed PostPartRelationshipsAccessRulesDataObject
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postPartRelationshipsAttachmentsMediaDecoder : Json.Decode.Decoder PostPartRelationshipsAttachmentsMedia
postPartRelationshipsAttachmentsMediaDecoder =
    Json.Decode.succeed PostPartRelationshipsAttachmentsMedia
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list postPartRelationshipsAttachmentsMediaDataObjectDecoder)


postPartRelationshipsAttachmentsMediaDataObjectDecoder : Json.Decode.Decoder PostPartRelationshipsAttachmentsMediaDataObject
postPartRelationshipsAttachmentsMediaDataObjectDecoder =
    Json.Decode.succeed PostPartRelationshipsAttachmentsMediaDataObject
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postPartRelationshipsAudioDecoder : Json.Decode.Decoder PostPartRelationshipsAudio
postPartRelationshipsAudioDecoder =
    Json.Decode.succeed PostPartRelationshipsAudio
        |> Json.Decode.Pipeline.required "data" postPartRelationshipsAudioDataDecoder
        |> Json.Decode.Pipeline.required "links" postPartRelationshipsAudioLinksDecoder


postPartRelationshipsAudioDataDecoder : Json.Decode.Decoder PostPartRelationshipsAudioData
postPartRelationshipsAudioDataDecoder =
    Json.Decode.succeed PostPartRelationshipsAudioData
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postPartRelationshipsAudioLinksDecoder : Json.Decode.Decoder PostPartRelationshipsAudioLinks
postPartRelationshipsAudioLinksDecoder =
    Json.Decode.succeed PostPartRelationshipsAudioLinks
        |> Json.Decode.Pipeline.required "related" Json.Decode.string


postPartRelationshipsAudioPreviewDecoder : Json.Decode.Decoder PostPartRelationshipsAudioPreview
postPartRelationshipsAudioPreviewDecoder =
    Json.Decode.succeed PostPartRelationshipsAudioPreview
        |> Json.Decode.Pipeline.required "data" postPartRelationshipsAudioPreviewDataDecoder
        |> Json.Decode.Pipeline.required "links" postPartRelationshipsAudioPreviewLinksDecoder


postPartRelationshipsAudioPreviewDataDecoder : Json.Decode.Decoder PostPartRelationshipsAudioPreviewData
postPartRelationshipsAudioPreviewDataDecoder =
    Json.Decode.succeed PostPartRelationshipsAudioPreviewData
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postPartRelationshipsAudioPreviewLinksDecoder : Json.Decode.Decoder PostPartRelationshipsAudioPreviewLinks
postPartRelationshipsAudioPreviewLinksDecoder =
    Json.Decode.succeed PostPartRelationshipsAudioPreviewLinks
        |> Json.Decode.Pipeline.required "related" Json.Decode.string


postPartRelationshipsCampaignDecoder : Json.Decode.Decoder PostPartRelationshipsCampaign
postPartRelationshipsCampaignDecoder =
    Json.Decode.succeed PostPartRelationshipsCampaign
        |> Json.Decode.Pipeline.required "data" postPartRelationshipsCampaignDataDecoder
        |> Json.Decode.Pipeline.required "links" postPartRelationshipsCampaignLinksDecoder


postPartRelationshipsCampaignDataDecoder : Json.Decode.Decoder PostPartRelationshipsCampaignData
postPartRelationshipsCampaignDataDecoder =
    Json.Decode.succeed PostPartRelationshipsCampaignData
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postPartRelationshipsCampaignLinksDecoder : Json.Decode.Decoder PostPartRelationshipsCampaignLinks
postPartRelationshipsCampaignLinksDecoder =
    Json.Decode.succeed PostPartRelationshipsCampaignLinks
        |> Json.Decode.Pipeline.required "related" Json.Decode.string


postPartRelationshipsMediaDecoder : Json.Decode.Decoder PostPartRelationshipsMedia
postPartRelationshipsMediaDecoder =
    Json.Decode.succeed PostPartRelationshipsMedia
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list postPartRelationshipsMediaDataObjectDecoder)


postPartRelationshipsMediaDataObjectDecoder : Json.Decode.Decoder PostPartRelationshipsMediaDataObject
postPartRelationshipsMediaDataObjectDecoder =
    Json.Decode.succeed PostPartRelationshipsMediaDataObject
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postPartRelationshipsUserDecoder : Json.Decode.Decoder PostPartRelationshipsUser
postPartRelationshipsUserDecoder =
    Json.Decode.succeed PostPartRelationshipsUser
        |> Json.Decode.Pipeline.required "data" postPartRelationshipsUserDataDecoder
        |> Json.Decode.Pipeline.required "links" postPartRelationshipsUserLinksDecoder


postPartRelationshipsUserDataDecoder : Json.Decode.Decoder PostPartRelationshipsUserData
postPartRelationshipsUserDataDecoder =
    Json.Decode.succeed PostPartRelationshipsUserData
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postPartRelationshipsUserLinksDecoder : Json.Decode.Decoder PostPartRelationshipsUserLinks
postPartRelationshipsUserLinksDecoder =
    Json.Decode.succeed PostPartRelationshipsUserLinks
        |> Json.Decode.Pipeline.required "related" Json.Decode.string


postPartRelationshipsUserDefinedTagsDecoder : Json.Decode.Decoder PostPartRelationshipsUserDefinedTags
postPartRelationshipsUserDefinedTagsDecoder =
    Json.Decode.succeed PostPartRelationshipsUserDefinedTags
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list postPartRelationshipsUserDefinedTagsDataObjectDecoder)


postPartRelationshipsUserDefinedTagsDataObjectDecoder : Json.Decode.Decoder PostPartRelationshipsUserDefinedTagsDataObject
postPartRelationshipsUserDefinedTagsDataObjectDecoder =
    Json.Decode.succeed PostPartRelationshipsUserDefinedTagsDataObject
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


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
        |> Json.Decode.Pipeline.required "image_colors" postChunkAttributesPostFileImageColorsDecoder
        |> Json.Decode.Pipeline.required "media_id" Json.Decode.int
        |> Json.Decode.Pipeline.required "state" Json.Decode.string
        |> Json.Decode.Pipeline.required "url" Json.Decode.string
        |> Json.Decode.Pipeline.required "width" Json.Decode.int


postChunkAttributesPostFileImageColorsDecoder : Json.Decode.Decoder PostChunkAttributesPostFileImageColors
postChunkAttributesPostFileImageColorsDecoder =
    Json.Decode.succeed PostChunkAttributesPostFileImageColors
        |> Json.Decode.Pipeline.required "average_colors_of_corners" postChunkAttributesPostFileImageColorsAverageColorsOfCornersDecoder
        |> Json.Decode.Pipeline.required "dominant_color" Json.Decode.string
        |> Json.Decode.Pipeline.required "palette" (Json.Decode.list Json.Decode.string)
        |> Json.Decode.Pipeline.required "text_color" Json.Decode.string


postChunkAttributesPostFileImageColorsAverageColorsOfCornersDecoder : Json.Decode.Decoder PostChunkAttributesPostFileImageColorsAverageColorsOfCorners
postChunkAttributesPostFileImageColorsAverageColorsOfCornersDecoder =
    Json.Decode.succeed PostChunkAttributesPostFileImageColorsAverageColorsOfCorners
        |> Json.Decode.Pipeline.required "bottom_left" Json.Decode.string
        |> Json.Decode.Pipeline.required "bottom_right" Json.Decode.string
        |> Json.Decode.Pipeline.required "top_left" Json.Decode.string
        |> Json.Decode.Pipeline.required "top_right" Json.Decode.string


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
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list postChunkRelationshipsAccessRulesDataObjectDecoder)


postChunkRelationshipsAccessRulesDataObjectDecoder : Json.Decode.Decoder PostChunkRelationshipsAccessRulesDataObject
postChunkRelationshipsAccessRulesDataObjectDecoder =
    Json.Decode.succeed PostChunkRelationshipsAccessRulesDataObject
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postChunkRelationshipsCampaignDecoder : Json.Decode.Decoder PostChunkRelationshipsCampaign
postChunkRelationshipsCampaignDecoder =
    Json.Decode.succeed PostChunkRelationshipsCampaign
        |> Json.Decode.Pipeline.required "data" postChunkRelationshipsCampaignDataDecoder
        |> Json.Decode.Pipeline.required "links" postChunkRelationshipsCampaignLinksDecoder


postChunkRelationshipsCampaignDataDecoder : Json.Decode.Decoder PostChunkRelationshipsCampaignData
postChunkRelationshipsCampaignDataDecoder =
    Json.Decode.succeed PostChunkRelationshipsCampaignData
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postChunkRelationshipsCampaignLinksDecoder : Json.Decode.Decoder PostChunkRelationshipsCampaignLinks
postChunkRelationshipsCampaignLinksDecoder =
    Json.Decode.succeed PostChunkRelationshipsCampaignLinks
        |> Json.Decode.Pipeline.required "related" Json.Decode.string


postChunkRelationshipsImagesDecoder : Json.Decode.Decoder PostChunkRelationshipsImages
postChunkRelationshipsImagesDecoder =
    Json.Decode.succeed PostChunkRelationshipsImages
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list postChunkRelationshipsImagesDataObjectDecoder)


postChunkRelationshipsImagesDataObjectDecoder : Json.Decode.Decoder PostChunkRelationshipsImagesDataObject
postChunkRelationshipsImagesDataObjectDecoder =
    Json.Decode.succeed PostChunkRelationshipsImagesDataObject
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postChunkRelationshipsMediaDecoder : Json.Decode.Decoder PostChunkRelationshipsMedia
postChunkRelationshipsMediaDecoder =
    Json.Decode.succeed PostChunkRelationshipsMedia
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list postChunkRelationshipsMediaDataObjectDecoder)


postChunkRelationshipsMediaDataObjectDecoder : Json.Decode.Decoder PostChunkRelationshipsMediaDataObject
postChunkRelationshipsMediaDataObjectDecoder =
    Json.Decode.succeed PostChunkRelationshipsMediaDataObject
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postChunkRelationshipsUserDecoder : Json.Decode.Decoder PostChunkRelationshipsUser
postChunkRelationshipsUserDecoder =
    Json.Decode.succeed PostChunkRelationshipsUser
        |> Json.Decode.Pipeline.required "data" postChunkRelationshipsUserDataDecoder
        |> Json.Decode.Pipeline.required "links" postChunkRelationshipsUserLinksDecoder


postChunkRelationshipsUserDataDecoder : Json.Decode.Decoder PostChunkRelationshipsUserData
postChunkRelationshipsUserDataDecoder =
    Json.Decode.succeed PostChunkRelationshipsUserData
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


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
        |> Json.Decode.Pipeline.required "image_colors" postPieceAttributesPostFileImageColorsDecoder
        |> Json.Decode.Pipeline.required "media_id" Json.Decode.int
        |> Json.Decode.Pipeline.required "state" Json.Decode.string
        |> Json.Decode.Pipeline.required "url" Json.Decode.string
        |> Json.Decode.Pipeline.required "width" Json.Decode.int


postPieceAttributesPostFileImageColorsDecoder : Json.Decode.Decoder PostPieceAttributesPostFileImageColors
postPieceAttributesPostFileImageColorsDecoder =
    Json.Decode.succeed PostPieceAttributesPostFileImageColors
        |> Json.Decode.Pipeline.required "average_colors_of_corners" postPieceAttributesPostFileImageColorsAverageColorsOfCornersDecoder
        |> Json.Decode.Pipeline.required "dominant_color" Json.Decode.string
        |> Json.Decode.Pipeline.required "palette" (Json.Decode.list Json.Decode.string)
        |> Json.Decode.Pipeline.required "text_color" Json.Decode.string


postPieceAttributesPostFileImageColorsAverageColorsOfCornersDecoder : Json.Decode.Decoder PostPieceAttributesPostFileImageColorsAverageColorsOfCorners
postPieceAttributesPostFileImageColorsAverageColorsOfCornersDecoder =
    Json.Decode.succeed PostPieceAttributesPostFileImageColorsAverageColorsOfCorners
        |> Json.Decode.Pipeline.required "bottom_left" Json.Decode.string
        |> Json.Decode.Pipeline.required "bottom_right" Json.Decode.string
        |> Json.Decode.Pipeline.required "top_left" Json.Decode.string
        |> Json.Decode.Pipeline.required "top_right" Json.Decode.string


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
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list postPieceRelationshipsAccessRulesDataObjectDecoder)


postPieceRelationshipsAccessRulesDataObjectDecoder : Json.Decode.Decoder PostPieceRelationshipsAccessRulesDataObject
postPieceRelationshipsAccessRulesDataObjectDecoder =
    Json.Decode.succeed PostPieceRelationshipsAccessRulesDataObject
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postPieceRelationshipsCampaignDecoder : Json.Decode.Decoder PostPieceRelationshipsCampaign
postPieceRelationshipsCampaignDecoder =
    Json.Decode.succeed PostPieceRelationshipsCampaign
        |> Json.Decode.Pipeline.required "data" postPieceRelationshipsCampaignDataDecoder
        |> Json.Decode.Pipeline.required "links" postPieceRelationshipsCampaignLinksDecoder


postPieceRelationshipsCampaignDataDecoder : Json.Decode.Decoder PostPieceRelationshipsCampaignData
postPieceRelationshipsCampaignDataDecoder =
    Json.Decode.succeed PostPieceRelationshipsCampaignData
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postPieceRelationshipsCampaignLinksDecoder : Json.Decode.Decoder PostPieceRelationshipsCampaignLinks
postPieceRelationshipsCampaignLinksDecoder =
    Json.Decode.succeed PostPieceRelationshipsCampaignLinks
        |> Json.Decode.Pipeline.required "related" Json.Decode.string


postPieceRelationshipsImagesDecoder : Json.Decode.Decoder PostPieceRelationshipsImages
postPieceRelationshipsImagesDecoder =
    Json.Decode.succeed PostPieceRelationshipsImages
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list postPieceRelationshipsImagesDataObjectDecoder)


postPieceRelationshipsImagesDataObjectDecoder : Json.Decode.Decoder PostPieceRelationshipsImagesDataObject
postPieceRelationshipsImagesDataObjectDecoder =
    Json.Decode.succeed PostPieceRelationshipsImagesDataObject
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postPieceRelationshipsMediaDecoder : Json.Decode.Decoder PostPieceRelationshipsMedia
postPieceRelationshipsMediaDecoder =
    Json.Decode.succeed PostPieceRelationshipsMedia
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list postPieceRelationshipsMediaDataObjectDecoder)


postPieceRelationshipsMediaDataObjectDecoder : Json.Decode.Decoder PostPieceRelationshipsMediaDataObject
postPieceRelationshipsMediaDataObjectDecoder =
    Json.Decode.succeed PostPieceRelationshipsMediaDataObject
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postPieceRelationshipsUserDecoder : Json.Decode.Decoder PostPieceRelationshipsUser
postPieceRelationshipsUserDecoder =
    Json.Decode.succeed PostPieceRelationshipsUser
        |> Json.Decode.Pipeline.required "data" postPieceRelationshipsUserDataDecoder
        |> Json.Decode.Pipeline.required "links" postPieceRelationshipsUserLinksDecoder


postPieceRelationshipsUserDataDecoder : Json.Decode.Decoder PostPieceRelationshipsUserData
postPieceRelationshipsUserDataDecoder =
    Json.Decode.succeed PostPieceRelationshipsUserData
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


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
        |> Json.Decode.Pipeline.required "image_colors" postThingyAttributesPostFileImageColorsDecoder
        |> Json.Decode.Pipeline.required "media_id" Json.Decode.int
        |> Json.Decode.Pipeline.required "state" Json.Decode.string
        |> Json.Decode.Pipeline.required "url" Json.Decode.string
        |> Json.Decode.Pipeline.required "width" Json.Decode.int


postThingyAttributesPostFileImageColorsDecoder : Json.Decode.Decoder PostThingyAttributesPostFileImageColors
postThingyAttributesPostFileImageColorsDecoder =
    Json.Decode.succeed PostThingyAttributesPostFileImageColors
        |> Json.Decode.Pipeline.required "average_colors_of_corners" postThingyAttributesPostFileImageColorsAverageColorsOfCornersDecoder
        |> Json.Decode.Pipeline.required "dominant_color" Json.Decode.string
        |> Json.Decode.Pipeline.required "palette" (Json.Decode.list Json.Decode.string)
        |> Json.Decode.Pipeline.required "text_color" Json.Decode.string


postThingyAttributesPostFileImageColorsAverageColorsOfCornersDecoder : Json.Decode.Decoder PostThingyAttributesPostFileImageColorsAverageColorsOfCorners
postThingyAttributesPostFileImageColorsAverageColorsOfCornersDecoder =
    Json.Decode.succeed PostThingyAttributesPostFileImageColorsAverageColorsOfCorners
        |> Json.Decode.Pipeline.required "bottom_left" Json.Decode.string
        |> Json.Decode.Pipeline.required "bottom_right" Json.Decode.string
        |> Json.Decode.Pipeline.required "top_left" Json.Decode.string
        |> Json.Decode.Pipeline.required "top_right" Json.Decode.string


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
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list postThingyRelationshipsAccessRulesDataObjectDecoder)


postThingyRelationshipsAccessRulesDataObjectDecoder : Json.Decode.Decoder PostThingyRelationshipsAccessRulesDataObject
postThingyRelationshipsAccessRulesDataObjectDecoder =
    Json.Decode.succeed PostThingyRelationshipsAccessRulesDataObject
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postThingyRelationshipsCampaignDecoder : Json.Decode.Decoder PostThingyRelationshipsCampaign
postThingyRelationshipsCampaignDecoder =
    Json.Decode.succeed PostThingyRelationshipsCampaign
        |> Json.Decode.Pipeline.required "data" postThingyRelationshipsCampaignDataDecoder
        |> Json.Decode.Pipeline.required "links" postThingyRelationshipsCampaignLinksDecoder


postThingyRelationshipsCampaignDataDecoder : Json.Decode.Decoder PostThingyRelationshipsCampaignData
postThingyRelationshipsCampaignDataDecoder =
    Json.Decode.succeed PostThingyRelationshipsCampaignData
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postThingyRelationshipsCampaignLinksDecoder : Json.Decode.Decoder PostThingyRelationshipsCampaignLinks
postThingyRelationshipsCampaignLinksDecoder =
    Json.Decode.succeed PostThingyRelationshipsCampaignLinks
        |> Json.Decode.Pipeline.required "related" Json.Decode.string


postThingyRelationshipsImagesDecoder : Json.Decode.Decoder PostThingyRelationshipsImages
postThingyRelationshipsImagesDecoder =
    Json.Decode.succeed PostThingyRelationshipsImages
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list postThingyRelationshipsImagesDataObjectDecoder)


postThingyRelationshipsImagesDataObjectDecoder : Json.Decode.Decoder PostThingyRelationshipsImagesDataObject
postThingyRelationshipsImagesDataObjectDecoder =
    Json.Decode.succeed PostThingyRelationshipsImagesDataObject
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postThingyRelationshipsMediaDecoder : Json.Decode.Decoder PostThingyRelationshipsMedia
postThingyRelationshipsMediaDecoder =
    Json.Decode.succeed PostThingyRelationshipsMedia
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list postThingyRelationshipsMediaDataObjectDecoder)


postThingyRelationshipsMediaDataObjectDecoder : Json.Decode.Decoder PostThingyRelationshipsMediaDataObject
postThingyRelationshipsMediaDataObjectDecoder =
    Json.Decode.succeed PostThingyRelationshipsMediaDataObject
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postThingyRelationshipsUserDecoder : Json.Decode.Decoder PostThingyRelationshipsUser
postThingyRelationshipsUserDecoder =
    Json.Decode.succeed PostThingyRelationshipsUser
        |> Json.Decode.Pipeline.required "data" postThingyRelationshipsUserDataDecoder
        |> Json.Decode.Pipeline.required "links" postThingyRelationshipsUserLinksDecoder


postThingyRelationshipsUserDataDecoder : Json.Decode.Decoder PostThingyRelationshipsUserData
postThingyRelationshipsUserDataDecoder =
    Json.Decode.succeed PostThingyRelationshipsUserData
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


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
        |> Json.Decode.Pipeline.required "post_file" postThingamajigAttributesPostFileDecoder
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


postThingamajigAttributesPostFileDecoder : Json.Decode.Decoder PostThingamajigAttributesPostFile
postThingamajigAttributesPostFileDecoder =
    Json.Decode.succeed PostThingamajigAttributesPostFile
        |> Json.Decode.Pipeline.required "default_thumbnail" hasUrlDecoder
        |> Json.Decode.Pipeline.required "duration" Json.Decode.int
        |> Json.Decode.Pipeline.required "full_content_duration" Json.Decode.int
        |> Json.Decode.Pipeline.required "media_id" Json.Decode.int
        |> Json.Decode.Pipeline.required "progress" progressDecoder
        |> Json.Decode.Pipeline.required "state" Json.Decode.string
        |> Json.Decode.Pipeline.required "url" Json.Decode.string


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
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list postThingamajigRelationshipsAccessRulesDataObjectDecoder)


postThingamajigRelationshipsAccessRulesDataObjectDecoder : Json.Decode.Decoder PostThingamajigRelationshipsAccessRulesDataObject
postThingamajigRelationshipsAccessRulesDataObjectDecoder =
    Json.Decode.succeed PostThingamajigRelationshipsAccessRulesDataObject
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postThingamajigRelationshipsAttachmentsMediaDecoder : Json.Decode.Decoder PostThingamajigRelationshipsAttachmentsMedia
postThingamajigRelationshipsAttachmentsMediaDecoder =
    Json.Decode.succeed PostThingamajigRelationshipsAttachmentsMedia
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list postThingamajigRelationshipsAttachmentsMediaDataObjectDecoder)


postThingamajigRelationshipsAttachmentsMediaDataObjectDecoder : Json.Decode.Decoder PostThingamajigRelationshipsAttachmentsMediaDataObject
postThingamajigRelationshipsAttachmentsMediaDataObjectDecoder =
    Json.Decode.succeed PostThingamajigRelationshipsAttachmentsMediaDataObject
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postThingamajigRelationshipsAudioDecoder : Json.Decode.Decoder PostThingamajigRelationshipsAudio
postThingamajigRelationshipsAudioDecoder =
    Json.Decode.succeed PostThingamajigRelationshipsAudio
        |> Json.Decode.Pipeline.required "data" postThingamajigRelationshipsAudioDataDecoder
        |> Json.Decode.Pipeline.required "links" postThingamajigRelationshipsAudioLinksDecoder


postThingamajigRelationshipsAudioDataDecoder : Json.Decode.Decoder PostThingamajigRelationshipsAudioData
postThingamajigRelationshipsAudioDataDecoder =
    Json.Decode.succeed PostThingamajigRelationshipsAudioData
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postThingamajigRelationshipsAudioLinksDecoder : Json.Decode.Decoder PostThingamajigRelationshipsAudioLinks
postThingamajigRelationshipsAudioLinksDecoder =
    Json.Decode.succeed PostThingamajigRelationshipsAudioLinks
        |> Json.Decode.Pipeline.required "related" Json.Decode.string


postThingamajigRelationshipsAudioPreviewDecoder : Json.Decode.Decoder PostThingamajigRelationshipsAudioPreview
postThingamajigRelationshipsAudioPreviewDecoder =
    Json.Decode.succeed PostThingamajigRelationshipsAudioPreview
        |> Json.Decode.Pipeline.required "data" postThingamajigRelationshipsAudioPreviewDataDecoder
        |> Json.Decode.Pipeline.required "links" postThingamajigRelationshipsAudioPreviewLinksDecoder


postThingamajigRelationshipsAudioPreviewDataDecoder : Json.Decode.Decoder PostThingamajigRelationshipsAudioPreviewData
postThingamajigRelationshipsAudioPreviewDataDecoder =
    Json.Decode.succeed PostThingamajigRelationshipsAudioPreviewData
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postThingamajigRelationshipsAudioPreviewLinksDecoder : Json.Decode.Decoder PostThingamajigRelationshipsAudioPreviewLinks
postThingamajigRelationshipsAudioPreviewLinksDecoder =
    Json.Decode.succeed PostThingamajigRelationshipsAudioPreviewLinks
        |> Json.Decode.Pipeline.required "related" Json.Decode.string


postThingamajigRelationshipsCampaignDecoder : Json.Decode.Decoder PostThingamajigRelationshipsCampaign
postThingamajigRelationshipsCampaignDecoder =
    Json.Decode.succeed PostThingamajigRelationshipsCampaign
        |> Json.Decode.Pipeline.required "data" postThingamajigRelationshipsCampaignDataDecoder
        |> Json.Decode.Pipeline.required "links" postThingamajigRelationshipsCampaignLinksDecoder


postThingamajigRelationshipsCampaignDataDecoder : Json.Decode.Decoder PostThingamajigRelationshipsCampaignData
postThingamajigRelationshipsCampaignDataDecoder =
    Json.Decode.succeed PostThingamajigRelationshipsCampaignData
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postThingamajigRelationshipsCampaignLinksDecoder : Json.Decode.Decoder PostThingamajigRelationshipsCampaignLinks
postThingamajigRelationshipsCampaignLinksDecoder =
    Json.Decode.succeed PostThingamajigRelationshipsCampaignLinks
        |> Json.Decode.Pipeline.required "related" Json.Decode.string


postThingamajigRelationshipsImagesDecoder : Json.Decode.Decoder PostThingamajigRelationshipsImages
postThingamajigRelationshipsImagesDecoder =
    Json.Decode.succeed PostThingamajigRelationshipsImages
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list postThingamajigRelationshipsImagesDataObjectDecoder)


postThingamajigRelationshipsImagesDataObjectDecoder : Json.Decode.Decoder PostThingamajigRelationshipsImagesDataObject
postThingamajigRelationshipsImagesDataObjectDecoder =
    Json.Decode.succeed PostThingamajigRelationshipsImagesDataObject
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postThingamajigRelationshipsMediaDecoder : Json.Decode.Decoder PostThingamajigRelationshipsMedia
postThingamajigRelationshipsMediaDecoder =
    Json.Decode.succeed PostThingamajigRelationshipsMedia
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list postThingamajigRelationshipsMediaDataObjectDecoder)


postThingamajigRelationshipsMediaDataObjectDecoder : Json.Decode.Decoder PostThingamajigRelationshipsMediaDataObject
postThingamajigRelationshipsMediaDataObjectDecoder =
    Json.Decode.succeed PostThingamajigRelationshipsMediaDataObject
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postThingamajigRelationshipsUserDecoder : Json.Decode.Decoder PostThingamajigRelationshipsUser
postThingamajigRelationshipsUserDecoder =
    Json.Decode.succeed PostThingamajigRelationshipsUser
        |> Json.Decode.Pipeline.required "data" postThingamajigRelationshipsUserDataDecoder
        |> Json.Decode.Pipeline.required "links" postThingamajigRelationshipsUserLinksDecoder


postThingamajigRelationshipsUserDataDecoder : Json.Decode.Decoder PostThingamajigRelationshipsUserData
postThingamajigRelationshipsUserDataDecoder =
    Json.Decode.succeed PostThingamajigRelationshipsUserData
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


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
        |> Json.Decode.Pipeline.required "image_colors" postWhatsitAttributesPostFileImageColorsDecoder
        |> Json.Decode.Pipeline.required "media_id" Json.Decode.int
        |> Json.Decode.Pipeline.required "state" Json.Decode.string
        |> Json.Decode.Pipeline.required "url" Json.Decode.string
        |> Json.Decode.Pipeline.required "width" Json.Decode.int


postWhatsitAttributesPostFileImageColorsDecoder : Json.Decode.Decoder PostWhatsitAttributesPostFileImageColors
postWhatsitAttributesPostFileImageColorsDecoder =
    Json.Decode.succeed PostWhatsitAttributesPostFileImageColors
        |> Json.Decode.Pipeline.required "average_colors_of_corners" postWhatsitAttributesPostFileImageColorsAverageColorsOfCornersDecoder
        |> Json.Decode.Pipeline.required "dominant_color" Json.Decode.string
        |> Json.Decode.Pipeline.required "palette" (Json.Decode.list Json.Decode.string)
        |> Json.Decode.Pipeline.required "text_color" Json.Decode.string


postWhatsitAttributesPostFileImageColorsAverageColorsOfCornersDecoder : Json.Decode.Decoder PostWhatsitAttributesPostFileImageColorsAverageColorsOfCorners
postWhatsitAttributesPostFileImageColorsAverageColorsOfCornersDecoder =
    Json.Decode.succeed PostWhatsitAttributesPostFileImageColorsAverageColorsOfCorners
        |> Json.Decode.Pipeline.required "bottom_left" Json.Decode.string
        |> Json.Decode.Pipeline.required "bottom_right" Json.Decode.string
        |> Json.Decode.Pipeline.required "top_left" Json.Decode.string
        |> Json.Decode.Pipeline.required "top_right" Json.Decode.string


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
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list postWhatsitRelationshipsAccessRulesDataObjectDecoder)


postWhatsitRelationshipsAccessRulesDataObjectDecoder : Json.Decode.Decoder PostWhatsitRelationshipsAccessRulesDataObject
postWhatsitRelationshipsAccessRulesDataObjectDecoder =
    Json.Decode.succeed PostWhatsitRelationshipsAccessRulesDataObject
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postWhatsitRelationshipsCampaignDecoder : Json.Decode.Decoder PostWhatsitRelationshipsCampaign
postWhatsitRelationshipsCampaignDecoder =
    Json.Decode.succeed PostWhatsitRelationshipsCampaign
        |> Json.Decode.Pipeline.required "data" postWhatsitRelationshipsCampaignDataDecoder
        |> Json.Decode.Pipeline.required "links" postWhatsitRelationshipsCampaignLinksDecoder


postWhatsitRelationshipsCampaignDataDecoder : Json.Decode.Decoder PostWhatsitRelationshipsCampaignData
postWhatsitRelationshipsCampaignDataDecoder =
    Json.Decode.succeed PostWhatsitRelationshipsCampaignData
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postWhatsitRelationshipsCampaignLinksDecoder : Json.Decode.Decoder PostWhatsitRelationshipsCampaignLinks
postWhatsitRelationshipsCampaignLinksDecoder =
    Json.Decode.succeed PostWhatsitRelationshipsCampaignLinks
        |> Json.Decode.Pipeline.required "related" Json.Decode.string


postWhatsitRelationshipsImagesDecoder : Json.Decode.Decoder PostWhatsitRelationshipsImages
postWhatsitRelationshipsImagesDecoder =
    Json.Decode.succeed PostWhatsitRelationshipsImages
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list postWhatsitRelationshipsImagesDataObjectDecoder)


postWhatsitRelationshipsImagesDataObjectDecoder : Json.Decode.Decoder PostWhatsitRelationshipsImagesDataObject
postWhatsitRelationshipsImagesDataObjectDecoder =
    Json.Decode.succeed PostWhatsitRelationshipsImagesDataObject
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postWhatsitRelationshipsMediaDecoder : Json.Decode.Decoder PostWhatsitRelationshipsMedia
postWhatsitRelationshipsMediaDecoder =
    Json.Decode.succeed PostWhatsitRelationshipsMedia
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list postWhatsitRelationshipsMediaDataObjectDecoder)


postWhatsitRelationshipsMediaDataObjectDecoder : Json.Decode.Decoder PostWhatsitRelationshipsMediaDataObject
postWhatsitRelationshipsMediaDataObjectDecoder =
    Json.Decode.succeed PostWhatsitRelationshipsMediaDataObject
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postWhatsitRelationshipsUserDecoder : Json.Decode.Decoder PostWhatsitRelationshipsUser
postWhatsitRelationshipsUserDecoder =
    Json.Decode.succeed PostWhatsitRelationshipsUser
        |> Json.Decode.Pipeline.required "data" postWhatsitRelationshipsUserDataDecoder
        |> Json.Decode.Pipeline.required "links" postWhatsitRelationshipsUserLinksDecoder


postWhatsitRelationshipsUserDataDecoder : Json.Decode.Decoder PostWhatsitRelationshipsUserData
postWhatsitRelationshipsUserDataDecoder =
    Json.Decode.succeed PostWhatsitRelationshipsUserData
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


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
        |> Json.Decode.Pipeline.required "image_colors" postDoodadAttributesPostFileImageColorsDecoder
        |> Json.Decode.Pipeline.required "media_id" Json.Decode.int
        |> Json.Decode.Pipeline.required "state" Json.Decode.string
        |> Json.Decode.Pipeline.required "url" Json.Decode.string
        |> Json.Decode.Pipeline.required "width" Json.Decode.int


postDoodadAttributesPostFileImageColorsDecoder : Json.Decode.Decoder PostDoodadAttributesPostFileImageColors
postDoodadAttributesPostFileImageColorsDecoder =
    Json.Decode.succeed PostDoodadAttributesPostFileImageColors
        |> Json.Decode.Pipeline.required "average_colors_of_corners" postDoodadAttributesPostFileImageColorsAverageColorsOfCornersDecoder
        |> Json.Decode.Pipeline.required "dominant_color" Json.Decode.string
        |> Json.Decode.Pipeline.required "palette" (Json.Decode.list Json.Decode.string)
        |> Json.Decode.Pipeline.required "text_color" Json.Decode.string


postDoodadAttributesPostFileImageColorsAverageColorsOfCornersDecoder : Json.Decode.Decoder PostDoodadAttributesPostFileImageColorsAverageColorsOfCorners
postDoodadAttributesPostFileImageColorsAverageColorsOfCornersDecoder =
    Json.Decode.succeed PostDoodadAttributesPostFileImageColorsAverageColorsOfCorners
        |> Json.Decode.Pipeline.required "bottom_left" Json.Decode.string
        |> Json.Decode.Pipeline.required "bottom_right" Json.Decode.string
        |> Json.Decode.Pipeline.required "top_left" Json.Decode.string
        |> Json.Decode.Pipeline.required "top_right" Json.Decode.string


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
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list postDoodadRelationshipsAccessRulesDataObjectDecoder)


postDoodadRelationshipsAccessRulesDataObjectDecoder : Json.Decode.Decoder PostDoodadRelationshipsAccessRulesDataObject
postDoodadRelationshipsAccessRulesDataObjectDecoder =
    Json.Decode.succeed PostDoodadRelationshipsAccessRulesDataObject
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postDoodadRelationshipsCampaignDecoder : Json.Decode.Decoder PostDoodadRelationshipsCampaign
postDoodadRelationshipsCampaignDecoder =
    Json.Decode.succeed PostDoodadRelationshipsCampaign
        |> Json.Decode.Pipeline.required "data" postDoodadRelationshipsCampaignDataDecoder
        |> Json.Decode.Pipeline.required "links" postDoodadRelationshipsCampaignLinksDecoder


postDoodadRelationshipsCampaignDataDecoder : Json.Decode.Decoder PostDoodadRelationshipsCampaignData
postDoodadRelationshipsCampaignDataDecoder =
    Json.Decode.succeed PostDoodadRelationshipsCampaignData
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postDoodadRelationshipsCampaignLinksDecoder : Json.Decode.Decoder PostDoodadRelationshipsCampaignLinks
postDoodadRelationshipsCampaignLinksDecoder =
    Json.Decode.succeed PostDoodadRelationshipsCampaignLinks
        |> Json.Decode.Pipeline.required "related" Json.Decode.string


postDoodadRelationshipsImagesDecoder : Json.Decode.Decoder PostDoodadRelationshipsImages
postDoodadRelationshipsImagesDecoder =
    Json.Decode.succeed PostDoodadRelationshipsImages
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list postDoodadRelationshipsImagesDataObjectDecoder)


postDoodadRelationshipsImagesDataObjectDecoder : Json.Decode.Decoder PostDoodadRelationshipsImagesDataObject
postDoodadRelationshipsImagesDataObjectDecoder =
    Json.Decode.succeed PostDoodadRelationshipsImagesDataObject
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postDoodadRelationshipsMediaDecoder : Json.Decode.Decoder PostDoodadRelationshipsMedia
postDoodadRelationshipsMediaDecoder =
    Json.Decode.succeed PostDoodadRelationshipsMedia
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list postDoodadRelationshipsMediaDataObjectDecoder)


postDoodadRelationshipsMediaDataObjectDecoder : Json.Decode.Decoder PostDoodadRelationshipsMediaDataObject
postDoodadRelationshipsMediaDataObjectDecoder =
    Json.Decode.succeed PostDoodadRelationshipsMediaDataObject
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postDoodadRelationshipsUserDecoder : Json.Decode.Decoder PostDoodadRelationshipsUser
postDoodadRelationshipsUserDecoder =
    Json.Decode.succeed PostDoodadRelationshipsUser
        |> Json.Decode.Pipeline.required "data" postDoodadRelationshipsUserDataDecoder
        |> Json.Decode.Pipeline.required "links" postDoodadRelationshipsUserLinksDecoder


postDoodadRelationshipsUserDataDecoder : Json.Decode.Decoder PostDoodadRelationshipsUserData
postDoodadRelationshipsUserDataDecoder =
    Json.Decode.succeed PostDoodadRelationshipsUserData
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postDoodadRelationshipsUserLinksDecoder : Json.Decode.Decoder PostDoodadRelationshipsUserLinks
postDoodadRelationshipsUserLinksDecoder =
    Json.Decode.succeed PostDoodadRelationshipsUserLinks
        |> Json.Decode.Pipeline.required "related" Json.Decode.string
