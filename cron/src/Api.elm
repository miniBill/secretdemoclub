module Api exposing (AccessRule, Attributes, AudioLinks, AudioRelationships, AverageColorsOfCorners, Embed, IdAndType, Image, ImageColors, ImageUrls, Media, Post, PostAudioVideo, PostFile(..), PostImage, PostMetadata, PostSize, PostTag, PostType(..), Relationships, Reward, Thumbnail, getPosts)

import BackendTask exposing (BackendTask)
import BackendTask.Do as Do
import BackendTask.File as File
import BackendTask.Http as Http
import DecodeComplete
import FatalError exposing (FatalError)
import Json.Decode
import Pages.Script as Script
import Parser.Advanced
import Result.Extra
import Rfc3339
import SHA256
import SeqDict exposing (SeqDict)
import Time
import Url exposing (Url)
import Url.Builder


getPosts :
    { a | workDir : String, cookie : String }
    -> BackendTask FatalError (List Post)
getPosts cookie =
    let
        rawPosts : BackendTask FatalError ( List RawPost, SeqDict IdAndType RawIncluded )
        rawPosts =
            getPaginated cookie postsUrl rawPostDecoder rawIncludedDecoder
    in
    rawPosts
        |> BackendTask.andThen
            (\( posts, included ) ->
                posts
                    |> Result.Extra.combineMap (\rawPost -> rawPostToPost included rawPost)
                    |> BackendTask.fromResult
            )


rawPostToPost :
    SeqDict IdAndType RawIncluded
    -> RawPost
    -> Result FatalError Post
rawPostToPost included rawPost =
    Result.map
        (\relationships ->
            { id = rawPost.id
            , attributes = rawPost.attributes
            , relationships = relationships
            }
        )
        (rawRelationshipsToRelationships included rawPost.relationships)
        |> Result.mapError FatalError.fromString


rawRelationshipsToRelationships : SeqDict IdAndType RawIncluded -> RawRelationships -> Result String Relationships
rawRelationshipsToRelationships included rawRelationships =
    let
        find : IdAndType -> (RawIncluded -> Result String value) -> Result String value
        find key unpacker =
            case SeqDict.get key included of
                Nothing ->
                    Err "Could not find"

                Just value ->
                    unpacker value

        asAccessRule : RawIncluded -> Result String AccessRule
        asAccessRule value =
            case value of
                RawIncludedPostAccessRule raw data ->
                    case data.tier of
                        Nothing ->
                            { accessRuleType = raw.accessRuleType
                            , amountCents = raw.amountCents
                            , currency = raw.currency
                            , postCount = raw.postCount
                            , reward = Nothing
                            }
                                |> Ok

                        Just tier ->
                            find tier asReward
                                |> Result.map
                                    (\reward ->
                                        { accessRuleType = raw.accessRuleType
                                        , amountCents = raw.amountCents
                                        , currency = raw.currency
                                        , postCount = raw.postCount
                                        , reward = Just reward
                                        }
                                    )

                _ ->
                    Err "Value is not a valid access rule"

        asReward : RawIncluded -> Result String Reward
        asReward value =
            case value of
                RawIncludedReward reward ->
                    Ok reward

                _ ->
                    Err "Value is not a valid reward"

        asMedia : RawIncluded -> Result String Media
        asMedia value =
            case value of
                RawIncludedMedia media ->
                    Ok media

                _ ->
                    Err "Value is not a valid media"

        asTag : RawIncluded -> Result String PostTag
        asTag value =
            case value of
                RawIncludedPostTag media ->
                    Ok media

                _ ->
                    Err "Value is not a valid post tag"

        asContentUnlockOption : RawIncluded -> Result String {}
        asContentUnlockOption value =
            case value of
                RawIncludedContentUnlockOption ->
                    Ok {}

                _ ->
                    Err "Value is not a valid post tag"

        findAll : (RawRelationships -> List IdAndType) -> (RawIncluded -> Result String value) -> Result String (List value)
        findAll prop unpacker =
            prop rawRelationships
                |> Result.Extra.combineMap (\item -> find item unpacker)
    in
    Ok Relationships
        |> Result.Extra.andMap (findAll .accessRules asAccessRule)
        |> Result.Extra.andMap (findAll .attachmentsMedia asMedia)
        |> Result.Extra.andMap (Ok rawRelationships.audio)
        |> Result.Extra.andMap (Ok rawRelationships.audioPreview)
        |> Result.Extra.andMap (findAll .images asMedia)
        |> Result.Extra.andMap (findAll .video asMedia)
        |> Result.Extra.andMap (findAll .media asMedia)
        |> Result.Extra.andMap (findAll .userDefinedTags asTag)
        |> Result.Extra.andMap (findAll .contentUnlockOptions asContentUnlockOption)


type RawIncluded
    = RawIncludedMedia Media
    | RawIncludedPostTag PostTag
    | RawIncludedPostAccessRule RawAccessRule { tier : Maybe IdAndType }
    | RawIncludedContentUnlockOption
    | RawIncludedReward Reward


rawIncludedDecoder : String -> DecodeComplete.ObjectDecoder RawIncluded
rawIncludedDecoder type_ =
    case type_ of
        "media" ->
            DecodeComplete.object RawIncludedMedia
                |> DecodeComplete.required "attributes"
                    (DecodeComplete.object Media
                        |> DecodeComplete.required "display" postFileDecoder
                        |> DecodeComplete.required "download_url" Json.Decode.string
                        |> DecodeComplete.required "file_name" (Json.Decode.nullable Json.Decode.string)
                        |> DecodeComplete.required "image_urls" (Json.Decode.nullable imageUrlsDecoder)
                        |> DecodeComplete.discard "metadata"
                        |> DecodeComplete.complete
                    )

        "post_tag" ->
            DecodeComplete.object RawIncludedPostTag
                |> DecodeComplete.required "attributes"
                    (DecodeComplete.object identity
                        |> DecodeComplete.required "tag_type"
                            (Json.Decode.string
                                |> Json.Decode.andThen
                                    (\tagType ->
                                        case tagType of
                                            "user_defined" ->
                                                Json.Decode.succeed UserDefined

                                            _ ->
                                                Json.Decode.fail ("Unknown tag_type:" ++ tagType)
                                    )
                            )
                        |> DecodeComplete.required "value" Json.Decode.string
                        |> DecodeComplete.complete
                    )

        "access-rule" ->
            DecodeComplete.object RawIncludedPostAccessRule
                |> DecodeComplete.required "attributes"
                    (DecodeComplete.object RawAccessRule
                        |> DecodeComplete.required "access_rule_type" Json.Decode.string
                        |> DecodeComplete.required "amount_cents" (Json.Decode.nullable Json.Decode.int)
                        |> DecodeComplete.required "currency" Json.Decode.string
                        |> DecodeComplete.required "post_count" Json.Decode.int
                        |> DecodeComplete.complete
                    )
                |> DecodeComplete.required "relationships"
                    (DecodeComplete.object (\tier -> { tier = tier })
                        |> DecodeComplete.required "tier"
                            (DecodeComplete.object identity
                                |> DecodeComplete.required "data" (Json.Decode.nullable idAndTypeDecoder)
                                |> DecodeComplete.discardOptional "links"
                                |> DecodeComplete.complete
                            )
                        |> DecodeComplete.complete
                    )

        "content-unlock-option" ->
            DecodeComplete.object (\_ -> RawIncludedContentUnlockOption)
                |> DecodeComplete.required "attributes"
                    (DecodeComplete.object {}
                        |> DecodeComplete.complete
                    )

        "reward" ->
            DecodeComplete.object RawIncludedReward
                |> DecodeComplete.required "attributes"
                    (DecodeComplete.object Reward
                        |> DecodeComplete.required "amount" Json.Decode.int
                        |> DecodeComplete.required "amount_cents" Json.Decode.int
                        |> DecodeComplete.required "created_at" rfc3339Decoder
                        |> DecodeComplete.required "currency" Json.Decode.string
                        |> DecodeComplete.discard "declined_patron_count"
                        |> DecodeComplete.required "description" Json.Decode.string
                        |> DecodeComplete.required "discord_role_ids" (listOrNull Json.Decode.string)
                        |> DecodeComplete.required "edited_at" Json.Decode.string
                        |> DecodeComplete.required "image_url" (Json.Decode.nullable Json.Decode.string)
                        |> DecodeComplete.required "is_free_tier" Json.Decode.bool
                        |> DecodeComplete.required "patron_amount_cents" Json.Decode.int
                        |> DecodeComplete.required "patron_currency" Json.Decode.string
                        |> DecodeComplete.required "post_count" Json.Decode.int
                        |> DecodeComplete.required "published" Json.Decode.bool
                        |> DecodeComplete.required "published_at" rfc3339Decoder
                        |> DecodeComplete.required "remaining" (Json.Decode.nullable Json.Decode.int)
                        |> DecodeComplete.required "requires_shipping" Json.Decode.bool
                        |> DecodeComplete.required "title" Json.Decode.string
                        |> DecodeComplete.required "unpublished_at" (Json.Decode.nullable rfc3339Decoder)
                        |> DecodeComplete.required "url" Json.Decode.string
                        |> DecodeComplete.optional "welcome_message" (Json.Decode.map Just Json.Decode.string) Nothing
                        |> DecodeComplete.optional "welcome_message_unsafe" (Json.Decode.nullable Json.Decode.string) Nothing
                        |> DecodeComplete.optional "welcome_video_embed" (Json.Decode.nullable Json.Decode.string) Nothing
                        |> DecodeComplete.optional "welcome_video_url" (Json.Decode.nullable Json.Decode.string) Nothing
                        |> DecodeComplete.complete
                    )

        _ ->
            DecodeComplete.fail ("Unexpected type: " ++ type_)


type alias Media =
    { display : PostFile
    , downloadUrl : String
    , fileName : Maybe String
    , imageUrls : Maybe ImageUrls
    }


type alias RawAccessRule =
    { accessRuleType : String
    , amountCents : Maybe Int
    , currency : String
    , postCount : Int
    }


type alias AccessRule =
    { accessRuleType : String
    , amountCents : Maybe Int
    , currency : String
    , postCount : Int
    , reward : Maybe Reward
    }


type alias Reward =
    { amount : Int
    , amountCents : Int
    , createdAt : Time.Posix
    , currency : String
    , description : String
    , discordRoleIds : List String
    , editedAt : String
    , imageUrl : Maybe String
    , isFreeTier : Bool
    , patronAmountCents : Int
    , patronCurrency : String
    , postCount : Int
    , published : Bool
    , publishedAt : Time.Posix
    , remaining : Maybe Int
    , requiresShipping : Bool
    , title : String
    , unpublishedAt : Maybe Time.Posix
    , url : String
    , welcomeMessage : Maybe String
    , welcomeMessageUnsafe : Maybe String
    , welcomeVideoEmbed : Maybe String
    , welcomeVideoUrl : Maybe String
    }


type PostTag
    = UserDefined String


type alias ImageUrls =
    { default : Maybe String
    , defaultBlurred : Maybe String
    , defaultBlurredSmall : Maybe String
    , defaultLarge : Maybe String
    , defaultSmall : String
    , original : Maybe String
    , thumbnail : String
    , thumbnailLarge : Maybe String
    , thumbnailSmall : Maybe String
    , url : String
    }


imageUrlsDecoder : Json.Decode.Decoder ImageUrls
imageUrlsDecoder =
    DecodeComplete.object ImageUrls
        |> DecodeComplete.optional "default" (Json.Decode.map Just Json.Decode.string) Nothing
        |> DecodeComplete.optional "default_blurred" (Json.Decode.map Just Json.Decode.string) Nothing
        |> DecodeComplete.optional "default_blurred_small" (Json.Decode.map Just Json.Decode.string) Nothing
        |> DecodeComplete.optional "default_large" (Json.Decode.map Just Json.Decode.string) Nothing
        |> DecodeComplete.required "default_small" Json.Decode.string
        |> DecodeComplete.optional "original" (Json.Decode.map Just Json.Decode.string) Nothing
        |> DecodeComplete.required "thumbnail" Json.Decode.string
        |> DecodeComplete.optional "thumbnail_large" (Json.Decode.map Just Json.Decode.string) Nothing
        |> DecodeComplete.optional "thumbnail_small" (Json.Decode.map Just Json.Decode.string) Nothing
        |> DecodeComplete.required "url" Json.Decode.string
        |> DecodeComplete.complete


type alias Page data included =
    { data : List data
    , next : Maybe String
    , included : SeqDict IdAndType included
    }


getPaginated :
    { cfg | workDir : String, cookie : String }
    -> (String -> String)
    -> Json.Decode.Decoder data
    -> (String -> DecodeComplete.ObjectDecoder included)
    -> BackendTask FatalError ( List data, SeqDict IdAndType included )
getPaginated config toUrl dataDecoder includedDecoder =
    let
        pageDecoder : Json.Decode.Decoder (Page data included)
        pageDecoder =
            DecodeComplete.object
                (\data included next ->
                    { data = data
                    , next = next
                    , included = SeqDict.fromList included
                    }
                )
                |> DecodeComplete.required "data" (Json.Decode.list dataDecoder)
                |> DecodeComplete.required "included"
                    (Json.Decode.list
                        (DecodeComplete.object
                            (\id type_ ->
                                { id = id
                                , type_ = type_
                                }
                            )
                            |> DecodeComplete.required "id" Json.Decode.string
                            |> DecodeComplete.required "type" Json.Decode.string
                            |> DecodeComplete.andThen
                                (\key ->
                                    includedDecoder key.type_
                                        |> DecodeComplete.andThen
                                            (\value -> DecodeComplete.object ( key, value ))
                                )
                            |> DecodeComplete.complete
                        )
                    )
                |> DecodeComplete.required "meta"
                    (Json.Decode.at
                        [ "pagination", "cursors", "next" ]
                        (Json.Decode.nullable Json.Decode.string)
                    )
                |> DecodeComplete.discardOptional "links"
                |> DecodeComplete.complete

        decodeContent : String -> BackendTask FatalError (Page data included)
        decodeContent content =
            Json.Decode.decodeString pageDecoder content
                |> Result.mapError (Json.Decode.errorToString >> FatalError.fromString)
                |> BackendTask.fromResult

        go :
            String
            -> List ( List data, SeqDict IdAndType included )
            -> BackendTask FatalError ( List data, SeqDict IdAndType included )
        go cursor acc =
            let
                url : String
                url =
                    toUrl cursor

                hash : String
                hash =
                    SHA256.fromString url
                        |> SHA256.toHex

                filename : String
                filename =
                    hash ++ ".json"

                target : String
                target =
                    String.join "/" [ config.workDir, "internal-api", filename ]
            in
            Do.glob target <| \existing ->
            Do.do
                (if not (List.isEmpty existing) then
                    File.rawFile target
                        |> BackendTask.allowFatal

                 else
                    let
                        httpRequest : BackendTask { fatal : FatalError, recoverable : Http.Error } String
                        httpRequest =
                            Http.request
                                { url = url
                                , method = "GET"
                                , body = Http.emptyBody
                                , headers =
                                    [ ( "User-Agent", "Mozilla/5.0 (X11; Linux x86_64; rv:133.0) Gecko/20100101 Firefox/133.0" )
                                    , ( "Referer", "https://www.patreon.com/c/orlagartland/posts" )
                                    , ( "Cookie", config.cookie )
                                    ]
                                , retries = Nothing
                                , timeoutInMs = Nothing
                                }
                                Http.expectString

                        writeFile : String -> BackendTask { fatal : FatalError, recoverable : Script.Error } ()
                        writeFile body =
                            Script.writeFile
                                { path = target
                                , body = body
                                }
                    in
                    Do.allowFatal httpRequest <| \body ->
                    Do.allowFatal (writeFile body) <| \_ ->
                    BackendTask.succeed body
                )
            <| \content ->
            Do.do (decodeContent content) <| \{ next, data, included } ->
            let
                nextData : List ( List data, SeqDict IdAndType included )
                nextData =
                    ( data, included ) :: acc
            in
            case next of
                Just nextCursor ->
                    go nextCursor nextData

                Nothing ->
                    let
                        ( finalData, finalIncluded ) =
                            List.unzip nextData
                    in
                    ( finalData
                        |> List.reverse
                        |> List.concat
                        |> List.reverse
                    , List.foldl SeqDict.union SeqDict.empty finalIncluded
                    )
                        |> BackendTask.succeed
    in
    go "" []


postsUrl : String -> String
postsUrl cursor =
    let
        fields : String -> List String -> Url.Builder.QueryParameter
        fields type_ list =
            Url.Builder.string
                ("fields[" ++ type_ ++ "]")
                (String.join "," list)
    in
    Url.Builder.crossOrigin
        "https://www.patreon.com"
        [ "api", "posts" ]
        [ Url.Builder.string "include"
            ([ "access_rules"

             --  , "access_rules.tier.null"
             , "access_rules.tier"
             , "attachments_media"
             , "audio"
             , "audio_preview.null"
             , "images"
             , "media"
             , "user_defined_tags"
             , "video.null"
             , "content_unlock_options.product_variant.null"
             ]
                |> String.join ","
            )
        , fields "post"
            [ "change_visibility_at"
            , "content"
            , "created_at"
            , "embed"
            , "image"
            , "meta_image_url"
            , "post_file"
            , "post_metadata"
            , "published_at"
            , "patreon_url"
            , "post_type"
            , "pledge_url"
            , "preview_asset_type"
            , "thumbnail"
            , "thumbnail_url"
            , "title"
            , "url"
            , "video"
            , "video_preview"
            , "content_unlock_options"
            ]
        , fields "post_tag"
            [ "tag_type"
            , "value"
            ]
        , fields "access_rule"
            [ "access_rule_type"
            ]
        , fields "media"
            [ "id"
            , "image_urls"
            , "display"
            , "download_url"
            , "metadata"
            , "file_name"
            ]
        , fields "productvariant"
            [ "price_cents"
            , "currency_code"
            , "checkout_url"
            , "is_hidden"
            , "published_at_datetime"
            , "content_type"
            , "orders_count"
            , "access_metadata"
            ]
        , fields "contentunlock-option" [ "content_unlock_type" ]
        , Url.Builder.string "filter[campaign_id]" "119662"
        , Url.Builder.string "sort" "-published_at"
        , Url.Builder.string "page[cursor]" cursor
        , Url.Builder.string "json-api-use-default-includes" "false"
        , Url.Builder.string "json-api-version" "1.0"
        ]


type alias Post =
    { attributes : Attributes
    , id : String
    , relationships : Relationships
    }


type alias RawPost =
    { attributes : Attributes
    , id : String
    , relationships : RawRelationships
    }


rawPostDecoder : Json.Decode.Decoder RawPost
rawPostDecoder =
    DecodeComplete.object RawPost
        |> DecodeComplete.required "attributes" attributesDecoder
        |> DecodeComplete.required "id" Json.Decode.string
        |> DecodeComplete.required "relationships" relationshipsDecoder
        |> DecodeComplete.discard "type"
        |> DecodeComplete.complete


type alias Attributes =
    { content : Maybe String
    , createdAt : Time.Posix
    , embed : Maybe Embed
    , image : Maybe Image
    , metaImageUrl : Url
    , patreonUrl : String
    , pledgeUrl : String
    , postFile : Maybe PostFile
    , postMetadata : Maybe PostMetadata
    , postType : PostType
    , previewAssetType : Maybe String
    , publishedAt : Time.Posix
    , thumbnail : Maybe Thumbnail
    , title : Maybe String
    , url : Url
    }


type PostType
    = Podcast
    | LivestreamYoutube
    | TextOnly
    | ImageFile
    | Link
    | VideoEmbed
    | VideoExternalFile
    | Poll
    | LivestreamCrowdcast
    | AudioEmbed


type alias Embed =
    { description : Maybe String
    , html : Maybe String
    , provider : String
    , providerUrl : String
    , subject : String
    , url : Url
    }


type alias Image =
    { height : Int
    , largeUrl : Maybe Url
    , thumbSquareLargeUrl : Maybe Url
    , thumbSquareUrl : Maybe Url
    , thumbUrl : Maybe Url
    , url : Url
    , width : Int
    }


type PostFile
    = PostFileAudioVideo PostAudioVideo
    | PostFileImage PostImage
    | PostFileSize PostSize


type alias PostSize =
    { width : Int
    , height : Int
    }


type alias PostAudioVideo =
    { defaultThumbnail : Maybe Url
    , duration : Maybe Float
    , fullContentDuration : Maybe Float
    , mediaId : Int
    , state : String
    , url : Url
    , width : Maybe Int
    , height : Maybe Int
    }


type alias PostImage =
    { height : Int
    , imageColors : ImageColors
    , mediaId : Int
    , state : String
    , url : Url
    , width : Int
    }


type PostMetadata
    = MetadataNone
    | MetadataWithImageOrder (List String)
    | MetadataPodcast
        { episodeNumber : Int
        , season : Int
        }


type Thumbnail
    = Thumbnail_Square SquareThumbnail
    | Thumbnail_Gif GifThumbnail


type alias SquareThumbnail =
    { large : String
    , large2 : String
    , square : String
    , url : Url
    }


type alias GifThumbnail =
    { gif_url : Url
    , height : Int
    , url : Url
    , width : Int
    }


type alias Relationships =
    { accessRules : List AccessRule
    , attachmentsMedia : List Media
    , audio : Maybe AudioRelationships
    , audioPreview : Maybe AudioRelationships
    , images : List Media
    , video : List Media
    , media : List Media
    , userDefinedTags : List PostTag
    , contentUnlockOptions : List {}
    }


type alias RawRelationships =
    { accessRules : List IdAndType
    , attachmentsMedia : List IdAndType
    , audio : Maybe AudioRelationships
    , audioPreview : Maybe AudioRelationships
    , images : List IdAndType
    , video : List IdAndType
    , media : List IdAndType
    , userDefinedTags : List IdAndType
    , contentUnlockOptions : List IdAndType
    }


type alias IdAndType =
    { id : String
    , type_ : String
    }


type alias AudioRelationships =
    { data : IdAndType
    , links : AudioLinks
    }


type alias AudioLinks =
    { related : String
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


attributesDecoder : Json.Decode.Decoder Attributes
attributesDecoder =
    DecodeComplete.object Attributes
        |> DecodeComplete.optional "content" (Json.Decode.map Just Json.Decode.string) Nothing
        |> DecodeComplete.required "created_at" rfc3339Decoder
        |> DecodeComplete.optional "embed" (Json.Decode.map Just embedDecoder) Nothing
        |> DecodeComplete.optional "image" (Json.Decode.map Just imageDecoder) Nothing
        |> DecodeComplete.required "meta_image_url" urlDecoder
        |> DecodeComplete.required "patreon_url" Json.Decode.string
        |> DecodeComplete.required "pledge_url" Json.Decode.string
        |> DecodeComplete.optional "post_file" (Json.Decode.map Just postFileDecoder) Nothing
        |> DecodeComplete.optional "post_metadata" (Json.Decode.map Just postMetadataDecoder) Nothing
        |> DecodeComplete.required "post_type" postTypeDecoder
        |> DecodeComplete.optional "preview_asset_type" (Json.Decode.map Just Json.Decode.string) Nothing
        |> DecodeComplete.required "published_at" rfc3339Decoder
        |> DecodeComplete.optional "thumbnail" (Json.Decode.map Just thumbnailDecoder) Nothing
        |> DecodeComplete.required "title" (Json.Decode.nullable Json.Decode.string)
        |> DecodeComplete.required "url" urlDecoder
        |> DecodeComplete.discard "change_visibility_at"
        |> DecodeComplete.discard "video_preview"
        |> DecodeComplete.complete


postTypeDecoder : Json.Decode.Decoder PostType
postTypeDecoder =
    Json.Decode.string
        |> Json.Decode.andThen
            (\type_ ->
                case type_ of
                    "podcast" ->
                        Json.Decode.succeed Podcast

                    "livestream_youtube" ->
                        Json.Decode.succeed LivestreamYoutube

                    "text_only" ->
                        Json.Decode.succeed TextOnly

                    "image_file" ->
                        Json.Decode.succeed ImageFile

                    "link" ->
                        Json.Decode.succeed Link

                    "video_embed" ->
                        Json.Decode.succeed VideoEmbed

                    "video_external_file" ->
                        Json.Decode.succeed VideoExternalFile

                    "poll" ->
                        Json.Decode.succeed Poll

                    "livestream_crowdcast" ->
                        Json.Decode.succeed LivestreamCrowdcast

                    "audio_embed" ->
                        Json.Decode.succeed AudioEmbed

                    _ ->
                        Json.Decode.fail ("Unexpected post type: " ++ type_)
            )


rfc3339Decoder : Json.Decode.Decoder Time.Posix
rfc3339Decoder =
    Json.Decode.string
        |> Json.Decode.andThen
            (\raw ->
                case Parser.Advanced.run Rfc3339.dateTimeOffsetParser raw of
                    Ok { instant } ->
                        Json.Decode.succeed instant

                    Err _ ->
                        Json.Decode.fail ("Invalid timestamp: " ++ raw)
            )


imageDecoder : Json.Decode.Decoder Image
imageDecoder =
    DecodeComplete.object Image
        |> DecodeComplete.required "height" Json.Decode.int
        |> DecodeComplete.optional "large_url" (Json.Decode.map Just urlDecoder) Nothing
        |> DecodeComplete.optional "thumb_square_large_url" (Json.Decode.map Just urlDecoder) Nothing
        |> DecodeComplete.optional "thumb_square_url" (Json.Decode.map Just urlDecoder) Nothing
        |> DecodeComplete.optional "thumb_url" (Json.Decode.map Just urlDecoder) Nothing
        |> DecodeComplete.required "url" urlDecoder
        |> DecodeComplete.required "width" Json.Decode.int
        |> DecodeComplete.complete


postAudioVideoDecoder : Json.Decode.Decoder PostAudioVideo
postAudioVideoDecoder =
    DecodeComplete.object PostAudioVideo
        |> DecodeComplete.optional "default_thumbnail"
            (Json.Decode.map Just (Json.Decode.field "url" urlDecoder))
            Nothing
        |> DecodeComplete.optional "duration" (Json.Decode.map Just Json.Decode.float) Nothing
        |> DecodeComplete.optional "full_content_duration" (Json.Decode.map Just Json.Decode.float) Nothing
        |> DecodeComplete.required "media_id" Json.Decode.int
        |> DecodeComplete.required "state" Json.Decode.string
        |> DecodeComplete.required "url" urlDecoder
        |> DecodeComplete.optional "width" (Json.Decode.map Just Json.Decode.int) Nothing
        |> DecodeComplete.optional "height" (Json.Decode.map Just Json.Decode.int) Nothing
        |> DecodeComplete.discardOptional "progress"
        |> DecodeComplete.discardOptional "closed_captions_enabled"
        |> DecodeComplete.discardOptional "video_issues"
        |> DecodeComplete.discardOptional "expires_at"
        |> DecodeComplete.complete


postMetadataDecoder : Json.Decode.Decoder PostMetadata
postMetadataDecoder =
    Json.Decode.oneOf
        [ DecodeComplete.object MetadataWithImageOrder
            |> DecodeComplete.required "image_order" (Json.Decode.list Json.Decode.string)
            |> DecodeComplete.complete
        , DecodeComplete.object
            (\episodeNumber season ->
                { episodeNumber = episodeNumber
                , season = season
                }
                    |> MetadataPodcast
            )
            |> DecodeComplete.required "episode_number" Json.Decode.int
            |> DecodeComplete.required "season" Json.Decode.int
            |> DecodeComplete.complete
        , Json.Decode.succeed MetadataNone
        ]


relationshipsDecoder : Json.Decode.Decoder RawRelationships
relationshipsDecoder =
    DecodeComplete.object RawRelationships
        |> DecodeComplete.required "access_rules" listOfIdAndTypeDecoder
        |> DecodeComplete.optional "attachments_media" listOfIdAndTypeDecoder []
        |> DecodeComplete.optional "audio"
            (Json.Decode.oneOf
                [ Json.Decode.map Just audioRelationshipsDecoder
                , Json.Decode.field "data" (Json.Decode.null Nothing)
                ]
            )
            Nothing
        |> DecodeComplete.optional "audio_preview"
            (Json.Decode.oneOf
                [ Json.Decode.map Just audioRelationshipsDecoder
                , Json.Decode.field "data" (Json.Decode.null Nothing)
                ]
            )
            Nothing
        |> DecodeComplete.optional "images" listOfIdAndTypeDecoder []
        |> DecodeComplete.optional "video" listOfIdAndTypeDecoder []
        |> DecodeComplete.optional "media" listOfIdAndTypeDecoder []
        |> DecodeComplete.optional "user_defined_tags" listOfIdAndTypeDecoder []
        |> DecodeComplete.optional "content_unlock_options" listOfIdAndTypeDecoder []
        |> DecodeComplete.complete


idAndTypeDecoder : Json.Decode.Decoder IdAndType
idAndTypeDecoder =
    DecodeComplete.object IdAndType
        |> DecodeComplete.required "id" Json.Decode.string
        |> DecodeComplete.required "type" Json.Decode.string
        |> DecodeComplete.complete


embedDecoder : Json.Decode.Decoder Embed
embedDecoder =
    DecodeComplete.object Embed
        |> DecodeComplete.optional "description" (Json.Decode.map Just Json.Decode.string) Nothing
        |> DecodeComplete.optional "html" (Json.Decode.map Just Json.Decode.string) Nothing
        |> DecodeComplete.required "provider" Json.Decode.string
        |> DecodeComplete.required "provider_url" Json.Decode.string
        |> DecodeComplete.required "subject" Json.Decode.string
        |> DecodeComplete.required "url" urlDecoder
        |> DecodeComplete.complete


postFileDecoder : Json.Decode.Decoder PostFile
postFileDecoder =
    Json.Decode.oneOf
        [ Json.Decode.map PostFileAudioVideo postAudioVideoDecoder
        , Json.Decode.map PostFileImage postImageDecoder
        , Json.Decode.map PostFileSize postSizeDecoder
        ]


postSizeDecoder : Json.Decode.Decoder PostSize
postSizeDecoder =
    DecodeComplete.object PostSize
        |> DecodeComplete.required "height" Json.Decode.int
        |> DecodeComplete.required "width" Json.Decode.int
        |> DecodeComplete.complete


postImageDecoder : Json.Decode.Decoder PostImage
postImageDecoder =
    DecodeComplete.object PostImage
        |> DecodeComplete.required "height" Json.Decode.int
        |> DecodeComplete.required "image_colors" imageColorsDecoder
        |> DecodeComplete.required "media_id" Json.Decode.int
        |> DecodeComplete.required "state" Json.Decode.string
        |> DecodeComplete.required "url" urlDecoder
        |> DecodeComplete.required "width" Json.Decode.int
        |> DecodeComplete.complete


imageColorsDecoder : Json.Decode.Decoder ImageColors
imageColorsDecoder =
    DecodeComplete.object ImageColors
        |> DecodeComplete.required "average_colors_of_corners" averageColorsOfCornersDecoder
        |> DecodeComplete.required "dominant_color" Json.Decode.string
        |> DecodeComplete.required "palette" (Json.Decode.list Json.Decode.string)
        |> DecodeComplete.required "text_color" Json.Decode.string
        |> DecodeComplete.complete


averageColorsOfCornersDecoder : Json.Decode.Decoder AverageColorsOfCorners
averageColorsOfCornersDecoder =
    DecodeComplete.object AverageColorsOfCorners
        |> DecodeComplete.required "bottom_left" Json.Decode.string
        |> DecodeComplete.required "bottom_right" Json.Decode.string
        |> DecodeComplete.required "top_left" Json.Decode.string
        |> DecodeComplete.required "top_right" Json.Decode.string
        |> DecodeComplete.complete


thumbnailDecoder : Json.Decode.Decoder Thumbnail
thumbnailDecoder =
    Json.Decode.oneOf
        [ Json.Decode.map Thumbnail_Square squareThumbnailDecoder
        , Json.Decode.map Thumbnail_Gif gifThumbnailDecoder
        ]


squareThumbnailDecoder : Json.Decode.Decoder SquareThumbnail
squareThumbnailDecoder =
    DecodeComplete.object SquareThumbnail
        |> DecodeComplete.required "large" Json.Decode.string
        |> DecodeComplete.required "large_2" Json.Decode.string
        |> DecodeComplete.required "square" Json.Decode.string
        |> DecodeComplete.required "url" urlDecoder
        |> DecodeComplete.complete


gifThumbnailDecoder : Json.Decode.Decoder GifThumbnail
gifThumbnailDecoder =
    DecodeComplete.object GifThumbnail
        |> DecodeComplete.required "gif_url" urlDecoder
        |> DecodeComplete.required "height" Json.Decode.int
        |> DecodeComplete.required "url" urlDecoder
        |> DecodeComplete.required "width" Json.Decode.int
        |> DecodeComplete.complete


urlDecoder : Json.Decode.Decoder Url
urlDecoder =
    Json.Decode.string
        |> Json.Decode.andThen
            (\raw ->
                case Url.fromString raw of
                    Just url ->
                        Json.Decode.succeed url

                    Nothing ->
                        Json.Decode.fail "Not a valid URL"
            )


audioRelationshipsDecoder : Json.Decode.Decoder AudioRelationships
audioRelationshipsDecoder =
    DecodeComplete.object AudioRelationships
        |> DecodeComplete.required "data" idAndTypeDecoder
        |> DecodeComplete.required "links" audioLinksDecoder
        |> DecodeComplete.complete


audioLinksDecoder : Json.Decode.Decoder AudioLinks
audioLinksDecoder =
    DecodeComplete.object AudioLinks
        |> DecodeComplete.required "related" Json.Decode.string
        |> DecodeComplete.complete


listOfIdAndTypeDecoder : Json.Decode.Decoder (List IdAndType)
listOfIdAndTypeDecoder =
    DecodeComplete.object identity
        |> DecodeComplete.required "data"
            (listOrNull idAndTypeDecoder)
        |> DecodeComplete.complete


listOrNull : Json.Decode.Decoder a -> Json.Decode.Decoder (List a)
listOrNull inner =
    Json.Decode.oneOf
        [ Json.Decode.list inner
        , Json.Decode.null []
        ]
