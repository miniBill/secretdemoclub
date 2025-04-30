module Api exposing (Attributes, AudioLinks, AudioRelationships, AverageColorsOfCorners, Embed, IdAndType, Image, ImageColors, PostFile(..), PostImage, PostMetadata, RawPost, Relationships, Thumbnail, getPosts)

import BackendTask exposing (BackendTask)
import BackendTask.Do as Do
import BackendTask.File as File
import BackendTask.Http as Http
import DecodeComplete
import FatalError exposing (FatalError)
import Json.Decode
import Json.Encode
import Pages.Script as Script
import Parser.Advanced
import Rfc3339
import SHA256
import SeqDict exposing (SeqDict)
import Time
import Url exposing (Url)
import Url.Builder


getPosts :
    { a | workDir : String, cookie : String }
    -> BackendTask FatalError (List RawPost)
getPosts cookie =
    let
        rawPosts :
            BackendTask
                FatalError
                ( List RawPost
                , SeqDict { id : String, type_ : String } RawIncluded
                )
        rawPosts =
            getPaginated cookie postsUrl rawPostDecoder rawIncludedDecoder
    in
    rawPosts
        |> BackendTask.andThen
            (\raw ->
                BackendTask.succeed (Tuple.first raw)
            )


type RawIncluded
    = RawIncludedMedia IncludedMedia
    | RawIncludedPostTag IncludedPostTag
    | RawIncludedPostAccessRule IncludedAccessRule
    | RawIncludedContentUnlockOption
    | RawIncludedReward IncludedReward


type alias IncludedMedia =
    { display : PostFile
    , downloadUrl : String
    , fileName : Maybe String
    , imageUrls : Maybe ImageUrls
    }


type alias IncludedAccessRule =
    { accessRuleType : String
    , amountCents : Maybe Int
    , currency : String
    , postCount : Int
    }


type alias IncludedReward =
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


type IncludedPostTag
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


rawIncludedDecoder : String -> Json.Decode.Decoder RawIncluded
rawIncludedDecoder type_ =
    case type_ of
        "media" ->
            DecodeComplete.object IncludedMedia
                |> DecodeComplete.required "display" postFileDecoder
                |> DecodeComplete.required "download_url" Json.Decode.string
                |> DecodeComplete.required "file_name" (Json.Decode.nullable Json.Decode.string)
                |> DecodeComplete.required "image_urls" (Json.Decode.nullable imageUrlsDecoder)
                |> DecodeComplete.discard "metadata"
                |> DecodeComplete.complete
                |> Json.Decode.map RawIncludedMedia

        "post_tag" ->
            DecodeComplete.object identity
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
                |> Json.Decode.map RawIncludedPostTag

        "access-rule" ->
            DecodeComplete.object IncludedAccessRule
                |> DecodeComplete.required "access_rule_type" Json.Decode.string
                |> DecodeComplete.required "amount_cents" (Json.Decode.nullable Json.Decode.int)
                |> DecodeComplete.required "currency" Json.Decode.string
                |> DecodeComplete.required "post_count" Json.Decode.int
                |> DecodeComplete.complete
                |> Json.Decode.map RawIncludedPostAccessRule

        "content-unlock-option" ->
            DecodeComplete.object RawIncludedContentUnlockOption
                |> DecodeComplete.complete

        "reward" ->
            DecodeComplete.object IncludedReward
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
                |> Json.Decode.map RawIncludedReward

        _ ->
            let
                debug : Json.Decode.Decoder String
                debug =
                    Json.Decode.value
                        |> Json.Decode.map (\raw -> Json.Encode.encode 0 raw)
            in
            Json.Decode.dict debug
                |> Json.Decode.andThen (\raw -> Json.Decode.fail (type_ ++ ": " ++ Debug.toString raw))


type alias Page data included =
    { data : List data
    , next : Maybe String
    , included : SeqDict { id : String, type_ : String } included
    }


getPaginated :
    { cfg | workDir : String, cookie : String }
    -> (String -> String)
    -> Json.Decode.Decoder data
    -> (String -> Json.Decode.Decoder included)
    -> BackendTask FatalError ( List data, SeqDict { id : String, type_ : String } included )
getPaginated config toUrl dataDecoder includedDecoder =
    let
        pageDecoder : Json.Decode.Decoder (Page data included)
        pageDecoder =
            Json.Decode.map3
                (\data included next ->
                    { data = data
                    , next = next
                    , included = SeqDict.fromList included
                    }
                )
                (Json.Decode.field "data" (Json.Decode.list dataDecoder))
                (Json.Decode.field "included"
                    (Json.Decode.list
                        (Json.Decode.map2
                            (\id type_ ->
                                { id = id
                                , type_ = type_
                                }
                            )
                            (Json.Decode.field "id" Json.Decode.string)
                            (Json.Decode.field "type" Json.Decode.string)
                            |> Json.Decode.andThen
                                (\key ->
                                    Json.Decode.map (Tuple.pair key)
                                        (Json.Decode.field "attributes" (includedDecoder key.type_))
                                )
                        )
                    )
                )
                (Json.Decode.at
                    [ "meta", "pagination", "cursors", "next" ]
                    (Json.Decode.nullable Json.Decode.string)
                )

        decodeContent : String -> BackendTask FatalError (Page data included)
        decodeContent content =
            Json.Decode.decodeString pageDecoder content
                |> Result.mapError (Json.Decode.errorToString >> FatalError.fromString)
                |> BackendTask.fromResult

        go :
            String
            -> List ( List data, SeqDict { id : String, type_ : String } included )
            -> BackendTask FatalError ( List data, SeqDict { id : String, type_ : String } included )
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
                nextData : List ( List data, SeqDict { id : String, type_ : String } included )
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


type alias RawPost =
    { attributes : Attributes
    , id : String
    , relationships : Relationships
    , type_ : String
    }


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
    , postType : String
    , previewAssetType : Maybe String
    , publishedAt : Time.Posix
    , thumbnail : Maybe Thumbnail
    , title : Maybe String
    , url : Url
    }


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


rawPostDecoder : Json.Decode.Decoder RawPost
rawPostDecoder =
    DecodeComplete.object RawPost
        |> DecodeComplete.required "attributes" attributesDecoder
        |> DecodeComplete.required "id" Json.Decode.string
        |> DecodeComplete.required "relationships" relationshipsDecoder
        |> DecodeComplete.required "type" Json.Decode.string
        |> DecodeComplete.complete


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
        |> DecodeComplete.required "post_type" Json.Decode.string
        |> DecodeComplete.optional "preview_asset_type" (Json.Decode.map Just Json.Decode.string) Nothing
        |> DecodeComplete.required "published_at" rfc3339Decoder
        |> DecodeComplete.optional "thumbnail" (Json.Decode.map Just thumbnailDecoder) Nothing
        |> DecodeComplete.required "title" (Json.Decode.nullable Json.Decode.string)
        |> DecodeComplete.required "url" urlDecoder
        |> DecodeComplete.discard "change_visibility_at"
        |> DecodeComplete.discard "video_preview"
        |> DecodeComplete.complete


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


relationshipsDecoder : Json.Decode.Decoder Relationships
relationshipsDecoder =
    DecodeComplete.object Relationships
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
