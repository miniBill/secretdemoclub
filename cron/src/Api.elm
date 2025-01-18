module Api exposing (getPosts)

import BackendTask exposing (BackendTask)
import BackendTask.Http as Http
import FatalError exposing (FatalError)
import Json.Decode
import Json.Decode.Pipeline
import Url exposing (Url)
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
             , "access_rules.tier.null"
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
    { attributes : Attributes
    , id : String
    , relationships : PostObjectRelationships
    , type_ : String
    }


type alias Attributes =
    { content : String
    , createdAt : String
    , image : Image
    , metaImageUrl : Url
    , patreonUrl : String
    , pledgeUrl : String
    , postFile : PostFile
    , postMetadata : PostMetadata
    , postType : String
    , previewAssetType : String
    , publishedAt : String
    , thumbnail : Thumbnail
    , title : String
    , url : Url
    }


type alias Image =
    { height : Int
    , largeUrl : Url
    , thumbSquareLargeUrl : Url
    , thumbSquareUrl : Url
    , thumbUrl : Url
    , url : Url
    , width : Int
    }


type PostFile
    = PostFileVideo PostVideo
    | PostFileImage PostImage


type alias PostVideo =
    { defaultThumbnail : Url
    , duration : Int
    , fullContentDuration : Int
    , mediaId : Int
    , state : String
    , url : Url
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
    = MetadataWithImageOrder (List String)
    | MetadataPodcast
        { episodeNumber : Int
        , season : Int
        }


type alias Thumbnail =
    { large : String
    , large2 : String
    , square : String
    , url : Url
    }


type alias PostObjectRelationships =
    { accessRules : ListOfIdAndType
    , audio : AudioRelationships
    , images : ListOfIdAndType
    , media : ListOfIdAndType
    }


type alias ListOfIdAndType =
    { data : List IdAndType
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


type alias PostMember =
    { attributes : PostMemberAttributes
    , id : String
    , relationships : PostMemberRelationships
    , type_ : String
    }


type alias PostMemberAttributes =
    { content : String
    , createdAt : String
    , embed : PostMemberAttributesEmbed
    , image : Image
    , metaImageUrl : Url
    , patreonUrl : String
    , pledgeUrl : String
    , postFile : PostFile
    , postMetadata : PostMetadata
    , postType : String
    , previewAssetType : String
    , publishedAt : String
    , thumbnail : Thumbnail
    , title : String
    , url : Url
    }


type alias PostMemberAttributesEmbed =
    { description : String
    , html : String
    , provider : String
    , providerUrl : Url
    , subject : String
    , url : Url
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


type alias PostMemberRelationships =
    { accessRules : ListOfIdAndType
    , images : ListOfIdAndType
    , media : ListOfIdAndType
    }


type alias PostEntity =
    { attributes : PostEntityAttributes
    , id : String
    , relationships : PostEntityRelationships
    , type_ : String
    }


type alias PostEntityAttributes =
    { content : String
    , createdAt : String
    , metaImageUrl : Url
    , patreonUrl : String
    , pledgeUrl : String
    , postType : String
    , publishedAt : String
    , title : String
    , url : Url
    }


type alias PostEntityRelationships =
    { accessRules : ListOfIdAndType
    }


type alias PostThing =
    { attributes : Attributes
    , id : String
    , relationships : PostThingRelationships
    , type_ : String
    }


type alias PostThingRelationships =
    { accessRules : ListOfIdAndType
    , attachmentsMedia : ListOfIdAndType
    , audio : AudioRelationships
    , images : ListOfIdAndType
    , media : ListOfIdAndType
    }


type alias PostInstance =
    { attributes : Attributes
    , id : String
    , relationships : PostInstanceRelationships
    , type_ : String
    }


type alias PostInstanceRelationships =
    { accessRules : ListOfIdAndType
    , images : ListOfIdAndType
    , media : ListOfIdAndType
    }


type alias PostConstituent =
    { attributes : PostConstituentAttributes
    , id : String
    , relationships : PostConstituentRelationships
    , type_ : String
    }


type alias PostConstituentAttributes =
    { content : String
    , createdAt : String
    , image : Image
    , metaImageUrl : Url
    , patreonUrl : String
    , pledgeUrl : String
    , postFile : PostFile
    , postMetadata : PostMetadata
    , postType : String
    , publishedAt : String
    , thumbnail : Thumbnail
    , title : String
    , url : Url
    }


type alias PostConstituentRelationships =
    { accessRules : ListOfIdAndType
    , images : ListOfIdAndType
    , media : ListOfIdAndType
    }


type alias PostSpecimen =
    { attributes : PostSpecimenAttributes
    , id : String
    , relationships : PostSpecimenRelationships
    , type_ : String
    }


type alias PostSpecimenAttributes =
    { content : String
    , createdAt : String
    , embed : PostSpecimenAttributesEmbed
    , image : Image
    , metaImageUrl : Url
    , patreonUrl : String
    , pledgeUrl : String
    , postFile : PostFile
    , postMetadata : PostMetadata
    , postType : String
    , previewAssetType : String
    , publishedAt : String
    , thumbnail : Thumbnail
    , title : String
    , url : Url
    }


type alias PostSpecimenAttributesEmbed =
    { description : String
    , provider : String
    , providerUrl : Url
    , subject : String
    , url : Url
    }


type alias PostSpecimenRelationships =
    { accessRules : ListOfIdAndType
    , images : ListOfIdAndType
    , media : ListOfIdAndType
    , userDefinedTags : ListOfIdAndType
    }


type alias PostGadget =
    { attributes : Attributes
    , id : String
    , relationships : PostGadgetRelationships
    , type_ : String
    }


type alias PostGadgetRelationships =
    { accessRules : ListOfIdAndType
    , audio : AudioRelationships
    , media : ListOfIdAndType
    , userDefinedTags : ListOfIdAndType
    }


type alias PostWidget =
    { attributes : PostWidgetAttributes
    , id : String
    , relationships : PostWidgetRelationships
    , type_ : String
    }


type alias PostWidgetAttributes =
    { content : String
    , createdAt : String
    , embed : PostWidgetAttributesEmbed
    , image : Image
    , metaImageUrl : Url
    , patreonUrl : String
    , pledgeUrl : String
    , postFile : PostFile
    , postMetadata : PostMetadata
    , postType : String
    , previewAssetType : String
    , publishedAt : String
    , thumbnail : Thumbnail
    , title : String
    , url : Url
    }


type alias PostWidgetAttributesEmbed =
    { description : String
    , html : String
    , provider : String
    , providerUrl : Url
    , subject : String
    , url : Url
    }


type alias PostWidgetRelationships =
    { accessRules : ListOfIdAndType
    , images : ListOfIdAndType
    , media : ListOfIdAndType
    , userDefinedTags : ListOfIdAndType
    }


type alias PostGizmo =
    { attributes : Attributes
    , id : String
    , relationships : PostGizmoRelationships
    , type_ : String
    }


type alias PostGizmoRelationships =
    { accessRules : ListOfIdAndType
    , audio : AudioRelationships
    , audioPreview : AudioRelationships
    , images : ListOfIdAndType
    , media : ListOfIdAndType
    }


type alias PostPart =
    { attributes : Attributes
    , id : String
    , relationships : PostPartRelationships
    , type_ : String
    }


type alias PostPartRelationships =
    { accessRules : ListOfIdAndType
    , attachmentsMedia : ListOfIdAndType
    , audio : AudioRelationships
    , audioPreview : AudioRelationships
    , media : ListOfIdAndType
    , userDefinedTags : ListOfIdAndType
    }


type alias PostChunk =
    { attributes : PostChunkAttributes
    , id : String
    , relationships : PostChunkRelationships
    , type_ : String
    }


type alias PostChunkAttributes =
    { content : String
    , createdAt : String
    , embed : PostChunkAttributesEmbed
    , image : Image
    , metaImageUrl : Url
    , patreonUrl : String
    , pledgeUrl : String
    , postFile : PostFile
    , postMetadata : PostMetadata
    , postType : String
    , previewAssetType : String
    , publishedAt : String
    , thumbnail : Thumbnail
    , title : String
    , url : Url
    }


type alias PostChunkAttributesEmbed =
    { description : String
    , html : String
    , provider : String
    , providerUrl : Url
    , subject : String
    , url : Url
    }


type alias PostChunkRelationships =
    { accessRules : ListOfIdAndType
    , images : ListOfIdAndType
    , media : ListOfIdAndType
    }


type alias PostPiece =
    { attributes : PostPieceAttributes
    , id : String
    , relationships : PostPieceRelationships
    , type_ : String
    }


type alias PostPieceAttributes =
    { content : String
    , createdAt : String
    , embed : PostPieceAttributesEmbed
    , image : Image
    , metaImageUrl : Url
    , patreonUrl : String
    , pledgeUrl : String
    , postFile : PostFile
    , postMetadata : PostMetadata
    , postType : String
    , previewAssetType : String
    , publishedAt : String
    , thumbnail : Thumbnail
    , title : String
    , url : Url
    }


type alias PostPieceAttributesEmbed =
    { description : String
    , html : String
    , provider : String
    , providerUrl : Url
    , subject : String
    , url : Url
    }


type alias PostPieceRelationships =
    { accessRules : ListOfIdAndType
    , images : ListOfIdAndType
    , media : ListOfIdAndType
    }


type alias PostThingy =
    { attributes : Attributes
    , id : String
    , relationships : PostThingyRelationships
    , type_ : String
    }


type alias PostThingyRelationships =
    { accessRules : ListOfIdAndType
    , images : ListOfIdAndType
    , media : ListOfIdAndType
    }


type alias PostThingamajig =
    { attributes : Attributes
    , id : String
    , relationships : PostThingamajigRelationships
    , type_ : String
    }


type alias PostThingamajigRelationships =
    { accessRules : ListOfIdAndType
    , attachmentsMedia : ListOfIdAndType
    , audio : AudioRelationships
    , audioPreview : AudioRelationships
    , images : ListOfIdAndType
    , media : ListOfIdAndType
    }


type alias PostWhatsit =
    { attributes : Attributes
    , id : String
    , relationships : PostWhatsitRelationships
    , type_ : String
    }


type alias PostWhatsitRelationships =
    { accessRules : ListOfIdAndType
    , images : ListOfIdAndType
    , media : ListOfIdAndType
    }


type alias PostDoodad =
    { attributes : Attributes
    , id : String
    , relationships : PostDoodadRelationships
    , type_ : String
    }


type alias PostDoodadRelationships =
    { accessRules : ListOfIdAndType
    , images : ListOfIdAndType
    , media : ListOfIdAndType
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
        |> Json.Decode.Pipeline.required "attributes" attributesDecoder
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "relationships" postObjectRelationshipsDecoder
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


attributesDecoder : Json.Decode.Decoder Attributes
attributesDecoder =
    Json.Decode.succeed Attributes
        |> Json.Decode.Pipeline.required "content" Json.Decode.string
        |> Json.Decode.Pipeline.required "created_at" Json.Decode.string
        |> Json.Decode.Pipeline.required "image" imageDecoder
        |> Json.Decode.Pipeline.required "meta_image_url" urlDecoder
        |> Json.Decode.Pipeline.required "patreon_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "pledge_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "post_file" postFileDecoder
        |> Json.Decode.Pipeline.required "post_metadata" postMetadataDecoder
        |> Json.Decode.Pipeline.required "post_type" Json.Decode.string
        |> Json.Decode.Pipeline.required "preview_asset_type" Json.Decode.string
        |> Json.Decode.Pipeline.required "published_at" Json.Decode.string
        |> Json.Decode.Pipeline.required "thumbnail" thumbnailDecoder
        |> Json.Decode.Pipeline.required "title" Json.Decode.string
        |> Json.Decode.Pipeline.required "url" urlDecoder


imageDecoder : Json.Decode.Decoder Image
imageDecoder =
    Json.Decode.succeed Image
        |> Json.Decode.Pipeline.required "height" Json.Decode.int
        |> Json.Decode.Pipeline.required "large_url" urlDecoder
        |> Json.Decode.Pipeline.required "thumb_square_large_url" urlDecoder
        |> Json.Decode.Pipeline.required "thumb_square_url" urlDecoder
        |> Json.Decode.Pipeline.required "thumb_url" urlDecoder
        |> Json.Decode.Pipeline.required "url" urlDecoder
        |> Json.Decode.Pipeline.required "width" Json.Decode.int


postVideoDecoder : Json.Decode.Decoder PostVideo
postVideoDecoder =
    Json.Decode.succeed PostVideo
        |> Json.Decode.Pipeline.required "default_thumbnail" hasUrlDecoder
        |> Json.Decode.Pipeline.required "duration" Json.Decode.int
        |> Json.Decode.Pipeline.required "full_content_duration" Json.Decode.int
        |> Json.Decode.Pipeline.required "media_id" Json.Decode.int
        |> Json.Decode.Pipeline.required "state" Json.Decode.string
        |> Json.Decode.Pipeline.required "url" urlDecoder


postMetadataDecoder : Json.Decode.Decoder PostMetadata
postMetadataDecoder =
    Json.Decode.oneOf
        [ Json.Decode.succeed MetadataWithImageOrder
            |> Json.Decode.Pipeline.required "image_order" (Json.Decode.list Json.Decode.string)
        , Json.Decode.succeed
            (\episodeNumber season ->
                { episodeNumber = episodeNumber
                , season = season
                }
                    |> MetadataPodcast
            )
            |> Json.Decode.Pipeline.required "episode_number" Json.Decode.int
            |> Json.Decode.Pipeline.required "season" Json.Decode.int
        ]


postObjectRelationshipsDecoder : Json.Decode.Decoder PostObjectRelationships
postObjectRelationshipsDecoder =
    Json.Decode.succeed PostObjectRelationships
        |> Json.Decode.Pipeline.required "access_rules" listOfIdAndTypeDecoder
        |> Json.Decode.Pipeline.required "audio" audioRelationshipsDecoder
        |> Json.Decode.Pipeline.required "images" listOfIdAndTypeDecoder
        |> Json.Decode.Pipeline.required "media" listOfIdAndTypeDecoder


idAndTypeDecoder : Json.Decode.Decoder IdAndType
idAndTypeDecoder =
    Json.Decode.succeed IdAndType
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


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
        |> Json.Decode.Pipeline.required "content" Json.Decode.string
        |> Json.Decode.Pipeline.required "created_at" Json.Decode.string
        |> Json.Decode.Pipeline.required "embed" postMemberAttributesEmbedDecoder
        |> Json.Decode.Pipeline.required "image" imageDecoder
        |> Json.Decode.Pipeline.required "meta_image_url" urlDecoder
        |> Json.Decode.Pipeline.required "patreon_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "pledge_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "post_file" postFileDecoder
        |> Json.Decode.Pipeline.required "post_metadata" postMetadataDecoder
        |> Json.Decode.Pipeline.required "post_type" Json.Decode.string
        |> Json.Decode.Pipeline.required "preview_asset_type" Json.Decode.string
        |> Json.Decode.Pipeline.required "published_at" Json.Decode.string
        |> Json.Decode.Pipeline.required "thumbnail" thumbnailDecoder
        |> Json.Decode.Pipeline.required "title" Json.Decode.string
        |> Json.Decode.Pipeline.required "url" urlDecoder


postMemberAttributesEmbedDecoder : Json.Decode.Decoder PostMemberAttributesEmbed
postMemberAttributesEmbedDecoder =
    Json.Decode.succeed PostMemberAttributesEmbed
        |> Json.Decode.Pipeline.required "description" Json.Decode.string
        |> Json.Decode.Pipeline.required "html" Json.Decode.string
        |> Json.Decode.Pipeline.required "provider" Json.Decode.string
        |> Json.Decode.Pipeline.required "provider_url" urlDecoder
        |> Json.Decode.Pipeline.required "subject" Json.Decode.string
        |> Json.Decode.Pipeline.required "url" urlDecoder


postFileDecoder : Json.Decode.Decoder PostFile
postFileDecoder =
    Json.Decode.oneOf
        [ Json.Decode.map PostFileVideo postVideoDecoder
        , Json.Decode.map PostFileImage postImageDecoder
        ]


postImageDecoder : Json.Decode.Decoder PostImage
postImageDecoder =
    Json.Decode.succeed PostImage
        |> Json.Decode.Pipeline.required "height" Json.Decode.int
        |> Json.Decode.Pipeline.required "image_colors" imageColorsDecoder
        |> Json.Decode.Pipeline.required "media_id" Json.Decode.int
        |> Json.Decode.Pipeline.required "state" Json.Decode.string
        |> Json.Decode.Pipeline.required "url" urlDecoder
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


thumbnailDecoder : Json.Decode.Decoder Thumbnail
thumbnailDecoder =
    Json.Decode.succeed Thumbnail
        |> Json.Decode.Pipeline.required "large" Json.Decode.string
        |> Json.Decode.Pipeline.required "large_2" Json.Decode.string
        |> Json.Decode.Pipeline.required "square" Json.Decode.string
        |> Json.Decode.Pipeline.required "url" urlDecoder


postMemberRelationshipsDecoder : Json.Decode.Decoder PostMemberRelationships
postMemberRelationshipsDecoder =
    Json.Decode.succeed PostMemberRelationships
        |> Json.Decode.Pipeline.required "access_rules" listOfIdAndTypeDecoder
        |> Json.Decode.Pipeline.required "images" listOfIdAndTypeDecoder
        |> Json.Decode.Pipeline.required "media" listOfIdAndTypeDecoder


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
        |> Json.Decode.Pipeline.required "content" Json.Decode.string
        |> Json.Decode.Pipeline.required "created_at" Json.Decode.string
        |> Json.Decode.Pipeline.required "meta_image_url" urlDecoder
        |> Json.Decode.Pipeline.required "patreon_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "pledge_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "post_type" Json.Decode.string
        |> Json.Decode.Pipeline.required "published_at" Json.Decode.string
        |> Json.Decode.Pipeline.required "title" Json.Decode.string
        |> Json.Decode.Pipeline.required "url" urlDecoder


postEntityRelationshipsDecoder : Json.Decode.Decoder PostEntityRelationships
postEntityRelationshipsDecoder =
    Json.Decode.succeed PostEntityRelationships
        |> Json.Decode.Pipeline.required "access_rules" listOfIdAndTypeDecoder


postThingDecoder : Json.Decode.Decoder PostThing
postThingDecoder =
    Json.Decode.succeed PostThing
        |> Json.Decode.Pipeline.required "attributes" attributesDecoder
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "relationships" postThingRelationshipsDecoder
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


hasUrlDecoder : Json.Decode.Decoder Url
hasUrlDecoder =
    Json.Decode.field "url" urlDecoder


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


postThingRelationshipsDecoder : Json.Decode.Decoder PostThingRelationships
postThingRelationshipsDecoder =
    Json.Decode.succeed PostThingRelationships
        |> Json.Decode.Pipeline.required "access_rules" listOfIdAndTypeDecoder
        |> Json.Decode.Pipeline.required "attachments_media" listOfIdAndTypeDecoder
        |> Json.Decode.Pipeline.required "audio" audioRelationshipsDecoder
        |> Json.Decode.Pipeline.required "images" listOfIdAndTypeDecoder
        |> Json.Decode.Pipeline.required "media" listOfIdAndTypeDecoder


audioRelationshipsDecoder : Json.Decode.Decoder AudioRelationships
audioRelationshipsDecoder =
    Json.Decode.succeed AudioRelationships
        |> Json.Decode.Pipeline.required "data" idAndTypeDecoder
        |> Json.Decode.Pipeline.required "links" audioLinksDecoder


audioLinksDecoder : Json.Decode.Decoder AudioLinks
audioLinksDecoder =
    Json.Decode.succeed AudioLinks
        |> Json.Decode.Pipeline.required "related" Json.Decode.string


postInstanceDecoder : Json.Decode.Decoder PostInstance
postInstanceDecoder =
    Json.Decode.succeed PostInstance
        |> Json.Decode.Pipeline.required "attributes" attributesDecoder
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "relationships" postInstanceRelationshipsDecoder
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postInstanceRelationshipsDecoder : Json.Decode.Decoder PostInstanceRelationships
postInstanceRelationshipsDecoder =
    Json.Decode.succeed PostInstanceRelationships
        |> Json.Decode.Pipeline.required "access_rules" listOfIdAndTypeDecoder
        |> Json.Decode.Pipeline.required "images" listOfIdAndTypeDecoder
        |> Json.Decode.Pipeline.required "media" listOfIdAndTypeDecoder


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
        |> Json.Decode.Pipeline.required "content" Json.Decode.string
        |> Json.Decode.Pipeline.required "created_at" Json.Decode.string
        |> Json.Decode.Pipeline.required "image" imageDecoder
        |> Json.Decode.Pipeline.required "meta_image_url" urlDecoder
        |> Json.Decode.Pipeline.required "patreon_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "pledge_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "post_file" postFileDecoder
        |> Json.Decode.Pipeline.required "post_metadata" postMetadataDecoder
        |> Json.Decode.Pipeline.required "post_type" Json.Decode.string
        |> Json.Decode.Pipeline.required "published_at" Json.Decode.string
        |> Json.Decode.Pipeline.required "thumbnail" thumbnailDecoder
        |> Json.Decode.Pipeline.required "title" Json.Decode.string
        |> Json.Decode.Pipeline.required "url" urlDecoder


postConstituentRelationshipsDecoder : Json.Decode.Decoder PostConstituentRelationships
postConstituentRelationshipsDecoder =
    Json.Decode.succeed PostConstituentRelationships
        |> Json.Decode.Pipeline.required "access_rules" listOfIdAndTypeDecoder
        |> Json.Decode.Pipeline.required "images" listOfIdAndTypeDecoder
        |> Json.Decode.Pipeline.required "media" listOfIdAndTypeDecoder


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
        |> Json.Decode.Pipeline.required "content" Json.Decode.string
        |> Json.Decode.Pipeline.required "created_at" Json.Decode.string
        |> Json.Decode.Pipeline.required "embed" postSpecimenAttributesEmbedDecoder
        |> Json.Decode.Pipeline.required "image" imageDecoder
        |> Json.Decode.Pipeline.required "meta_image_url" urlDecoder
        |> Json.Decode.Pipeline.required "patreon_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "pledge_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "post_file" postFileDecoder
        |> Json.Decode.Pipeline.required "post_metadata" postMetadataDecoder
        |> Json.Decode.Pipeline.required "post_type" Json.Decode.string
        |> Json.Decode.Pipeline.required "preview_asset_type" Json.Decode.string
        |> Json.Decode.Pipeline.required "published_at" Json.Decode.string
        |> Json.Decode.Pipeline.required "thumbnail" thumbnailDecoder
        |> Json.Decode.Pipeline.required "title" Json.Decode.string
        |> Json.Decode.Pipeline.required "url" urlDecoder


postSpecimenAttributesEmbedDecoder : Json.Decode.Decoder PostSpecimenAttributesEmbed
postSpecimenAttributesEmbedDecoder =
    Json.Decode.succeed PostSpecimenAttributesEmbed
        |> Json.Decode.Pipeline.required "description" Json.Decode.string
        |> Json.Decode.Pipeline.required "provider" Json.Decode.string
        |> Json.Decode.Pipeline.required "provider_url" urlDecoder
        |> Json.Decode.Pipeline.required "subject" Json.Decode.string
        |> Json.Decode.Pipeline.required "url" urlDecoder


postSpecimenRelationshipsDecoder : Json.Decode.Decoder PostSpecimenRelationships
postSpecimenRelationshipsDecoder =
    Json.Decode.succeed PostSpecimenRelationships
        |> Json.Decode.Pipeline.required "access_rules" listOfIdAndTypeDecoder
        |> Json.Decode.Pipeline.required "images" listOfIdAndTypeDecoder
        |> Json.Decode.Pipeline.required "media" listOfIdAndTypeDecoder
        |> Json.Decode.Pipeline.required "user_defined_tags" listOfIdAndTypeDecoder


postGadgetDecoder : Json.Decode.Decoder PostGadget
postGadgetDecoder =
    Json.Decode.succeed PostGadget
        |> Json.Decode.Pipeline.required "attributes" attributesDecoder
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "relationships" postGadgetRelationshipsDecoder
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postGadgetRelationshipsDecoder : Json.Decode.Decoder PostGadgetRelationships
postGadgetRelationshipsDecoder =
    Json.Decode.succeed PostGadgetRelationships
        |> Json.Decode.Pipeline.required "access_rules" listOfIdAndTypeDecoder
        |> Json.Decode.Pipeline.required "audio" audioRelationshipsDecoder
        |> Json.Decode.Pipeline.required "media" listOfIdAndTypeDecoder
        |> Json.Decode.Pipeline.required "user_defined_tags" listOfIdAndTypeDecoder


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
        |> Json.Decode.Pipeline.required "content" Json.Decode.string
        |> Json.Decode.Pipeline.required "created_at" Json.Decode.string
        |> Json.Decode.Pipeline.required "embed" postWidgetAttributesEmbedDecoder
        |> Json.Decode.Pipeline.required "image" imageDecoder
        |> Json.Decode.Pipeline.required "meta_image_url" urlDecoder
        |> Json.Decode.Pipeline.required "patreon_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "pledge_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "post_file" postFileDecoder
        |> Json.Decode.Pipeline.required "post_metadata" postMetadataDecoder
        |> Json.Decode.Pipeline.required "post_type" Json.Decode.string
        |> Json.Decode.Pipeline.required "preview_asset_type" Json.Decode.string
        |> Json.Decode.Pipeline.required "published_at" Json.Decode.string
        |> Json.Decode.Pipeline.required "thumbnail" thumbnailDecoder
        |> Json.Decode.Pipeline.required "title" Json.Decode.string
        |> Json.Decode.Pipeline.required "url" urlDecoder


postWidgetAttributesEmbedDecoder : Json.Decode.Decoder PostWidgetAttributesEmbed
postWidgetAttributesEmbedDecoder =
    Json.Decode.succeed PostWidgetAttributesEmbed
        |> Json.Decode.Pipeline.required "description" Json.Decode.string
        |> Json.Decode.Pipeline.required "html" Json.Decode.string
        |> Json.Decode.Pipeline.required "provider" Json.Decode.string
        |> Json.Decode.Pipeline.required "provider_url" urlDecoder
        |> Json.Decode.Pipeline.required "subject" Json.Decode.string
        |> Json.Decode.Pipeline.required "url" urlDecoder


postWidgetRelationshipsDecoder : Json.Decode.Decoder PostWidgetRelationships
postWidgetRelationshipsDecoder =
    Json.Decode.succeed PostWidgetRelationships
        |> Json.Decode.Pipeline.required "access_rules" listOfIdAndTypeDecoder
        |> Json.Decode.Pipeline.required "images" listOfIdAndTypeDecoder
        |> Json.Decode.Pipeline.required "media" listOfIdAndTypeDecoder
        |> Json.Decode.Pipeline.required "user_defined_tags" listOfIdAndTypeDecoder


listOfIdAndTypeDecoder : Json.Decode.Decoder ListOfIdAndType
listOfIdAndTypeDecoder =
    Json.Decode.succeed ListOfIdAndType
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list idAndTypeDecoder)


postGizmoDecoder : Json.Decode.Decoder PostGizmo
postGizmoDecoder =
    Json.Decode.succeed PostGizmo
        |> Json.Decode.Pipeline.required "attributes" attributesDecoder
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "relationships" postGizmoRelationshipsDecoder
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postGizmoRelationshipsDecoder : Json.Decode.Decoder PostGizmoRelationships
postGizmoRelationshipsDecoder =
    Json.Decode.succeed PostGizmoRelationships
        |> Json.Decode.Pipeline.required "access_rules" listOfIdAndTypeDecoder
        |> Json.Decode.Pipeline.required "audio" audioRelationshipsDecoder
        |> Json.Decode.Pipeline.required "audio_preview" audioRelationshipsDecoder
        |> Json.Decode.Pipeline.required "images" listOfIdAndTypeDecoder
        |> Json.Decode.Pipeline.required "media" listOfIdAndTypeDecoder


postPartDecoder : Json.Decode.Decoder PostPart
postPartDecoder =
    Json.Decode.succeed PostPart
        |> Json.Decode.Pipeline.required "attributes" attributesDecoder
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "relationships" postPartRelationshipsDecoder
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postPartRelationshipsDecoder : Json.Decode.Decoder PostPartRelationships
postPartRelationshipsDecoder =
    Json.Decode.succeed PostPartRelationships
        |> Json.Decode.Pipeline.required "access_rules" listOfIdAndTypeDecoder
        |> Json.Decode.Pipeline.required "attachments_media" listOfIdAndTypeDecoder
        |> Json.Decode.Pipeline.required "audio" audioRelationshipsDecoder
        |> Json.Decode.Pipeline.required "audio_preview" audioRelationshipsDecoder
        |> Json.Decode.Pipeline.required "media" listOfIdAndTypeDecoder
        |> Json.Decode.Pipeline.required "user_defined_tags" listOfIdAndTypeDecoder


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
        |> Json.Decode.Pipeline.required "content" Json.Decode.string
        |> Json.Decode.Pipeline.required "created_at" Json.Decode.string
        |> Json.Decode.Pipeline.required "embed" postChunkAttributesEmbedDecoder
        |> Json.Decode.Pipeline.required "image" imageDecoder
        |> Json.Decode.Pipeline.required "meta_image_url" urlDecoder
        |> Json.Decode.Pipeline.required "patreon_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "pledge_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "post_file" postFileDecoder
        |> Json.Decode.Pipeline.required "post_metadata" postMetadataDecoder
        |> Json.Decode.Pipeline.required "post_type" Json.Decode.string
        |> Json.Decode.Pipeline.required "preview_asset_type" Json.Decode.string
        |> Json.Decode.Pipeline.required "published_at" Json.Decode.string
        |> Json.Decode.Pipeline.required "thumbnail" thumbnailDecoder
        |> Json.Decode.Pipeline.required "title" Json.Decode.string
        |> Json.Decode.Pipeline.required "url" urlDecoder


postChunkAttributesEmbedDecoder : Json.Decode.Decoder PostChunkAttributesEmbed
postChunkAttributesEmbedDecoder =
    Json.Decode.succeed PostChunkAttributesEmbed
        |> Json.Decode.Pipeline.required "description" Json.Decode.string
        |> Json.Decode.Pipeline.required "html" Json.Decode.string
        |> Json.Decode.Pipeline.required "provider" Json.Decode.string
        |> Json.Decode.Pipeline.required "provider_url" urlDecoder
        |> Json.Decode.Pipeline.required "subject" Json.Decode.string
        |> Json.Decode.Pipeline.required "url" urlDecoder


postChunkRelationshipsDecoder : Json.Decode.Decoder PostChunkRelationships
postChunkRelationshipsDecoder =
    Json.Decode.succeed PostChunkRelationships
        |> Json.Decode.Pipeline.required "access_rules" listOfIdAndTypeDecoder
        |> Json.Decode.Pipeline.required "images" listOfIdAndTypeDecoder
        |> Json.Decode.Pipeline.required "media" listOfIdAndTypeDecoder


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
        |> Json.Decode.Pipeline.required "content" Json.Decode.string
        |> Json.Decode.Pipeline.required "created_at" Json.Decode.string
        |> Json.Decode.Pipeline.required "embed" postPieceAttributesEmbedDecoder
        |> Json.Decode.Pipeline.required "image" imageDecoder
        |> Json.Decode.Pipeline.required "meta_image_url" urlDecoder
        |> Json.Decode.Pipeline.required "patreon_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "pledge_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "post_file" postFileDecoder
        |> Json.Decode.Pipeline.required "post_metadata" postMetadataDecoder
        |> Json.Decode.Pipeline.required "post_type" Json.Decode.string
        |> Json.Decode.Pipeline.required "preview_asset_type" Json.Decode.string
        |> Json.Decode.Pipeline.required "published_at" Json.Decode.string
        |> Json.Decode.Pipeline.required "thumbnail" thumbnailDecoder
        |> Json.Decode.Pipeline.required "title" Json.Decode.string
        |> Json.Decode.Pipeline.required "url" urlDecoder


postPieceAttributesEmbedDecoder : Json.Decode.Decoder PostPieceAttributesEmbed
postPieceAttributesEmbedDecoder =
    Json.Decode.succeed PostPieceAttributesEmbed
        |> Json.Decode.Pipeline.required "description" Json.Decode.string
        |> Json.Decode.Pipeline.required "html" Json.Decode.string
        |> Json.Decode.Pipeline.required "provider" Json.Decode.string
        |> Json.Decode.Pipeline.required "provider_url" urlDecoder
        |> Json.Decode.Pipeline.required "subject" Json.Decode.string
        |> Json.Decode.Pipeline.required "url" urlDecoder


postPieceRelationshipsDecoder : Json.Decode.Decoder PostPieceRelationships
postPieceRelationshipsDecoder =
    Json.Decode.succeed PostPieceRelationships
        |> Json.Decode.Pipeline.required "access_rules" listOfIdAndTypeDecoder
        |> Json.Decode.Pipeline.required "images" listOfIdAndTypeDecoder
        |> Json.Decode.Pipeline.required "media" listOfIdAndTypeDecoder


postThingyDecoder : Json.Decode.Decoder PostThingy
postThingyDecoder =
    Json.Decode.succeed PostThingy
        |> Json.Decode.Pipeline.required "attributes" attributesDecoder
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "relationships" postThingyRelationshipsDecoder
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postThingyRelationshipsDecoder : Json.Decode.Decoder PostThingyRelationships
postThingyRelationshipsDecoder =
    Json.Decode.succeed PostThingyRelationships
        |> Json.Decode.Pipeline.required "access_rules" listOfIdAndTypeDecoder
        |> Json.Decode.Pipeline.required "images" listOfIdAndTypeDecoder
        |> Json.Decode.Pipeline.required "media" listOfIdAndTypeDecoder


postThingamajigDecoder : Json.Decode.Decoder PostThingamajig
postThingamajigDecoder =
    Json.Decode.succeed PostThingamajig
        |> Json.Decode.Pipeline.required "attributes" attributesDecoder
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "relationships" postThingamajigRelationshipsDecoder
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postThingamajigRelationshipsDecoder : Json.Decode.Decoder PostThingamajigRelationships
postThingamajigRelationshipsDecoder =
    Json.Decode.succeed PostThingamajigRelationships
        |> Json.Decode.Pipeline.required "access_rules" listOfIdAndTypeDecoder
        |> Json.Decode.Pipeline.required "attachments_media" listOfIdAndTypeDecoder
        |> Json.Decode.Pipeline.required "audio" audioRelationshipsDecoder
        |> Json.Decode.Pipeline.required "audio_preview" audioRelationshipsDecoder
        |> Json.Decode.Pipeline.required "images" listOfIdAndTypeDecoder
        |> Json.Decode.Pipeline.required "media" listOfIdAndTypeDecoder


postWhatsitDecoder : Json.Decode.Decoder PostWhatsit
postWhatsitDecoder =
    Json.Decode.succeed PostWhatsit
        |> Json.Decode.Pipeline.required "attributes" attributesDecoder
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "relationships" postWhatsitRelationshipsDecoder
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postWhatsitRelationshipsDecoder : Json.Decode.Decoder PostWhatsitRelationships
postWhatsitRelationshipsDecoder =
    Json.Decode.succeed PostWhatsitRelationships
        |> Json.Decode.Pipeline.required "access_rules" listOfIdAndTypeDecoder
        |> Json.Decode.Pipeline.required "images" listOfIdAndTypeDecoder
        |> Json.Decode.Pipeline.required "media" listOfIdAndTypeDecoder


postDoodadDecoder : Json.Decode.Decoder PostDoodad
postDoodadDecoder =
    Json.Decode.succeed PostDoodad
        |> Json.Decode.Pipeline.required "attributes" attributesDecoder
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "relationships" postDoodadRelationshipsDecoder
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


postDoodadRelationshipsDecoder : Json.Decode.Decoder PostDoodadRelationships
postDoodadRelationshipsDecoder =
    Json.Decode.succeed PostDoodadRelationships
        |> Json.Decode.Pipeline.required "access_rules" listOfIdAndTypeDecoder
        |> Json.Decode.Pipeline.required "images" listOfIdAndTypeDecoder
        |> Json.Decode.Pipeline.required "media" listOfIdAndTypeDecoder
