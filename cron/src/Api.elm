module Api exposing (getPosts)

import BackendTask exposing (BackendTask)
import BackendTask.Http as Http
import FatalError exposing (FatalError)
import Json.Decode
import Json.Encode
import Pages.Script as Script
import Url.Builder


getPosts :
    { a | cookie : String }
    -> BackendTask { fatal : FatalError, recoverable : Http.Error } (List Post)
getPosts { cookie } =
    let
        cursor =
            ""
    in
    Http.request
        { url =
            Url.Builder.crossOrigin
                "https://www.patreon.com"
                [ "api", "posts" ]
                [ Url.Builder.string "include" "campaign,access_rules,access_rules.tier.null,attachments_media,audio,audio_preview.null,drop,images,media,native_video_insights,poll.choices,poll.current_user_responses.user,poll.current_user_responses.choice,poll.current_user_responses.poll,user,user_defined_tags,ti_checks,video.null,content_unlock_options.product_variant.null"
                , Url.Builder.string "fields[campaign]" "currency,show_audio_post_download_links,avatar_photo_url,avatar_photo_image_urls,earnings_visibility,is_nsfw,is_monthly,name,url"
                , Url.Builder.string "fields[post]" "change_visibility_at,comment_count,commenter_count,content,created_at,current_user_can_comment,current_user_can_delete,current_user_can_report,current_user_can_view,current_user_comment_disallowed_reason,current_user_has_liked,embed,image,insights_last_updated_at,is_paid,like_count,meta_image_url,min_cents_pledged_to_view,monetization_ineligibility_reason,post_file,post_metadata,published_at,patreon_url,post_type,pledge_url,preview_asset_type,thumbnail,thumbnail_url,teaser_text,title,upgrade_url,url,was_posted_by_campaign_owner,has_ti_violation,moderation_status,post_level_suspension_removal_date,pls_one_liners_by_category,video,video_preview,view_count,content_unlock_options,is_new_to_current_user,watch_state"
                , Url.Builder.string "fields[post_tag]" "tag_type,value"
                , Url.Builder.string "fields[user]" "image_url,full_name,url"
                , Url.Builder.string "fields[access_rule]" "access_rule_type,amount_cents"
                , Url.Builder.string "fields[media]" "id,image_urls,display,download_url,metadata,file_name"
                , Url.Builder.string "fields[native_video_insights]" "average_view_duration,average_view_pct,has_preview,id,last_updated_at,num_views,preview_views,video_duration"
                , Url.Builder.string "fields[content-unlock-option]" "content_unlock_type"
                , Url.Builder.string "fields[product-variant]" "price_cents,currency_code,checkout_url,is_hidden,published_at_datetime,content_type,orders_count,access_metadata"
                , Url.Builder.string "filter[campaign_id]" "119662"
                , Url.Builder.string "filter[contains_exclusive_posts]" "true"
                , Url.Builder.string "filter[is_draft]" "false"
                , Url.Builder.string "filter[accessible_by_user_id]" "26157566"
                , Url.Builder.string "sort" "-published_at"
                , Url.Builder.string "page[cursor]" cursor
                , Url.Builder.string "json-api-use-default-includes" "false"
                , Url.Builder.string "json-api-version" "1.0"
                ]
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
        (Http.expectJson responseDecoder)
        |> BackendTask.map
            (\raw ->
                Script.writeFile
                    { path = "raw.json"
                    , body =
                        raw.data
                            |> Json.Encode.list identity
                            |> Json.Encode.encode 0
                    }
            )
        |> BackendTask.map
            (\raw -> [])


type alias Post =
    Json.Decode.Value


responseDecoder :
    Json.Decode.Decoder
        { meta : Pagination
        , data : List Post
        }
responseDecoder =
    Json.Decode.map2 (\data meta -> { data = data, meta = meta })
        (Json.Decode.field "data" (Json.Decode.list Json.Decode.value))
        (Json.Decode.field "meta"
            (Json.Decode.field "pagination"
                (Json.Decode.map2 Pagination
                    (Json.Decode.field "cursors"
                        (Json.Decode.field "next" (Json.Decode.nullable Json.Decode.string))
                    )
                    (Json.Decode.field "total" Json.Decode.int)
                )
            )
        )


type alias Pagination =
    { next : Maybe String
    , total : Int
    }
