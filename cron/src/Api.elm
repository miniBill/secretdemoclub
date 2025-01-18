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

        -- , Url.Builder.string "filter[contains_exclusive_posts]" "true"
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
    , progress : Progress
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


type alias Progress =
    { isWatched : Bool
    , watchState : String
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
    { accessRules : PostObjectRelationshipsAccessRules
    , audio : PostObjectRelationshipsAudio
    , images : PostObjectRelationshipsImages
    , media : PostObjectRelationshipsMedia
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
    { accessRules : PostMemberRelationshipsAccessRules
    , images : PostMemberRelationshipsImages
    , media : PostMemberRelationshipsMedia
    }


type alias PostMemberRelationshipsAccessRules =
    { data : List PostMemberRelationshipsAccessRulesDataObject
    }


type alias PostMemberRelationshipsAccessRulesDataObject =
    IdAndType


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
    { accessRules : PostEntityRelationshipsAccessRules
    }


type alias PostEntityRelationshipsAccessRules =
    { data : List PostEntityRelationshipsAccessRulesDataObject
    }


type alias PostEntityRelationshipsAccessRulesDataObject =
    IdAndType


type alias PostThing =
    { attributes : Attributes
    , id : String
    , relationships : PostThingRelationships
    , type_ : String
    }


type alias PostThingRelationships =
    { accessRules : PostThingRelationshipsAccessRules
    , attachmentsMedia : PostThingRelationshipsAttachmentsMedia
    , audio : PostThingRelationshipsAudio
    , images : PostThingRelationshipsImages
    , media : PostThingRelationshipsMedia
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


type alias PostInstance =
    { attributes : Attributes
    , id : String
    , relationships : PostInstanceRelationships
    , type_ : String
    }


type alias PostInstanceRelationships =
    { accessRules : PostInstanceRelationshipsAccessRules
    , images : PostInstanceRelationshipsImages
    , media : PostInstanceRelationshipsMedia
    }


type alias PostInstanceRelationshipsAccessRules =
    { data : List PostInstanceRelationshipsAccessRulesDataObject
    }


type alias PostInstanceRelationshipsAccessRulesDataObject =
    IdAndType


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
    { accessRules : PostConstituentRelationshipsAccessRules
    , images : PostConstituentRelationshipsImages
    , media : PostConstituentRelationshipsMedia
    }


type alias PostConstituentRelationshipsAccessRules =
    { data : List PostConstituentRelationshipsAccessRulesDataObject
    }


type alias PostConstituentRelationshipsAccessRulesDataObject =
    IdAndType


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
    { accessRules : PostSpecimenRelationshipsAccessRules
    , images : PostSpecimenRelationshipsImages
    , media : PostSpecimenRelationshipsMedia
    , userDefinedTags : PostSpecimenRelationshipsUserDefinedTags
    }


type alias PostSpecimenRelationshipsAccessRules =
    { data : List PostSpecimenRelationshipsAccessRulesDataObject
    }


type alias PostSpecimenRelationshipsAccessRulesDataObject =
    IdAndType


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


type alias PostSpecimenRelationshipsUserDefinedTags =
    { data : List PostSpecimenRelationshipsUserDefinedTagsDataObject
    }


type alias PostSpecimenRelationshipsUserDefinedTagsDataObject =
    IdAndType


type alias PostGadget =
    { attributes : Attributes
    , id : String
    , relationships : PostGadgetRelationships
    , type_ : String
    }


type alias PostGadgetRelationships =
    { accessRules : PostGadgetRelationshipsAccessRules
    , audio : PostGadgetRelationshipsAudio
    , media : PostGadgetRelationshipsMedia
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


type alias PostGadgetRelationshipsMedia =
    { data : List PostGadgetRelationshipsMediaDataObject
    }


type alias PostGadgetRelationshipsMediaDataObject =
    IdAndType


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
    { accessRules : PostWidgetRelationshipsAccessRules
    , images : PostWidgetRelationshipsImages
    , media : PostWidgetRelationshipsMedia
    , userDefinedTags : PostWidgetRelationshipsUserDefinedTags
    }


type alias PostWidgetRelationshipsAccessRules =
    ListOfIdAndType


type alias ListOfIdAndType =
    { data : List IdAndType
    }


type alias PostWidgetRelationshipsImages =
    ListOfIdAndType


type alias PostWidgetRelationshipsMedia =
    ListOfIdAndType


type alias PostWidgetRelationshipsUserDefinedTags =
    ListOfIdAndType


type alias PostGizmo =
    { attributes : Attributes
    , id : String
    , relationships : PostGizmoRelationships
    , type_ : String
    }


type alias PostGizmoRelationships =
    { accessRules : PostGizmoRelationshipsAccessRules
    , audio : PostGizmoRelationshipsAudio
    , audioPreview : PostGizmoRelationshipsAudioPreview
    , images : PostGizmoRelationshipsImages
    , media : PostGizmoRelationshipsMedia
    }


type alias PostGizmoRelationshipsAccessRules =
    ListOfIdAndType


type alias PostGizmoRelationshipsAudio =
    { data : IdAndType
    , links : PostGizmoRelationshipsAudioLinks
    }


type alias PostGizmoRelationshipsAudioLinks =
    { related : String
    }


type alias PostGizmoRelationshipsAudioPreview =
    { data : IdAndType
    , links : PostGizmoRelationshipsAudioPreviewLinks
    }


type alias PostGizmoRelationshipsAudioPreviewLinks =
    { related : String
    }


type alias PostGizmoRelationshipsImages =
    ListOfIdAndType


type alias PostGizmoRelationshipsMedia =
    ListOfIdAndType


type alias PostPart =
    { attributes : Attributes
    , id : String
    , relationships : PostPartRelationships
    , type_ : String
    }


type alias PostPartRelationships =
    { accessRules : PostPartRelationshipsAccessRules
    , attachmentsMedia : PostPartRelationshipsAttachmentsMedia
    , audio : PostPartRelationshipsAudio
    , audioPreview : PostPartRelationshipsAudioPreview
    , media : PostPartRelationshipsMedia
    , userDefinedTags : PostPartRelationshipsUserDefinedTags
    }


type alias PostPartRelationshipsAccessRules =
    ListOfIdAndType


type alias PostPartRelationshipsAttachmentsMedia =
    ListOfIdAndType


type alias PostPartRelationshipsAudio =
    { data : IdAndType
    , links : PostPartRelationshipsAudioLinks
    }


type alias PostPartRelationshipsAudioLinks =
    { related : String
    }


type alias PostPartRelationshipsAudioPreview =
    { data : IdAndType
    , links : PostPartRelationshipsAudioPreviewLinks
    }


type alias PostPartRelationshipsAudioPreviewLinks =
    { related : String
    }


type alias PostPartRelationshipsMedia =
    ListOfIdAndType


type alias PostPartRelationshipsUserDefinedTags =
    ListOfIdAndType


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
    { accessRules : PostChunkRelationshipsAccessRules
    , images : PostChunkRelationshipsImages
    , media : PostChunkRelationshipsMedia
    }


type alias PostChunkRelationshipsAccessRules =
    ListOfIdAndType


type alias PostChunkRelationshipsImages =
    ListOfIdAndType


type alias PostChunkRelationshipsMedia =
    ListOfIdAndType


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
    { accessRules : PostPieceRelationshipsAccessRules
    , images : PostPieceRelationshipsImages
    , media : PostPieceRelationshipsMedia
    }


type alias PostPieceRelationshipsAccessRules =
    ListOfIdAndType


type alias PostPieceRelationshipsImages =
    ListOfIdAndType


type alias PostPieceRelationshipsMedia =
    ListOfIdAndType


type alias PostThingy =
    { attributes : Attributes
    , id : String
    , relationships : PostThingyRelationships
    , type_ : String
    }


type alias PostThingyRelationships =
    { accessRules : PostThingyRelationshipsAccessRules
    , images : PostThingyRelationshipsImages
    , media : PostThingyRelationshipsMedia
    }


type alias PostThingyRelationshipsAccessRules =
    ListOfIdAndType


type alias PostThingyRelationshipsImages =
    ListOfIdAndType


type alias PostThingyRelationshipsMedia =
    ListOfIdAndType


type alias PostThingamajig =
    { attributes : Attributes
    , id : String
    , relationships : PostThingamajigRelationships
    , type_ : String
    }


type alias PostThingamajigRelationships =
    { accessRules : PostThingamajigRelationshipsAccessRules
    , attachmentsMedia : PostThingamajigRelationshipsAttachmentsMedia
    , audio : PostThingamajigRelationshipsAudio
    , audioPreview : PostThingamajigRelationshipsAudioPreview
    , images : PostThingamajigRelationshipsImages
    , media : PostThingamajigRelationshipsMedia
    }


type alias PostThingamajigRelationshipsAccessRules =
    ListOfIdAndType


type alias PostThingamajigRelationshipsAttachmentsMedia =
    ListOfIdAndType


type alias PostThingamajigRelationshipsAudio =
    { data : IdAndType
    , links : PostThingamajigRelationshipsAudioLinks
    }


type alias PostThingamajigRelationshipsAudioLinks =
    { related : String
    }


type alias PostThingamajigRelationshipsAudioPreview =
    { data : IdAndType
    , links : PostThingamajigRelationshipsAudioPreviewLinks
    }


type alias PostThingamajigRelationshipsAudioPreviewLinks =
    { related : String
    }


type alias PostThingamajigRelationshipsImages =
    ListOfIdAndType


type alias PostThingamajigRelationshipsMedia =
    ListOfIdAndType


type alias PostWhatsit =
    { attributes : Attributes
    , id : String
    , relationships : PostWhatsitRelationships
    , type_ : String
    }


type alias PostWhatsitRelationships =
    { accessRules : PostWhatsitRelationshipsAccessRules
    , images : PostWhatsitRelationshipsImages
    , media : PostWhatsitRelationshipsMedia
    }


type alias PostWhatsitRelationshipsAccessRules =
    ListOfIdAndType


type alias PostWhatsitRelationshipsImages =
    ListOfIdAndType


type alias PostWhatsitRelationshipsMedia =
    ListOfIdAndType


type alias PostDoodad =
    { attributes : Attributes
    , id : String
    , relationships : PostDoodadRelationships
    , type_ : String
    }


type alias PostDoodadRelationships =
    { accessRules : PostDoodadRelationshipsAccessRules
    , images : PostDoodadRelationshipsImages
    , media : PostDoodadRelationshipsMedia
    }


type alias PostDoodadRelationshipsAccessRules =
    ListOfIdAndType


type alias PostDoodadRelationshipsImages =
    ListOfIdAndType


type alias PostDoodadRelationshipsMedia =
    ListOfIdAndType


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
        |> Json.Decode.Pipeline.required "progress" progressDecoder
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
        |> Json.Decode.Pipeline.required "access_rules" postObjectRelationshipsAccessRulesDecoder
        |> Json.Decode.Pipeline.required "audio" postObjectRelationshipsAudioDecoder
        |> Json.Decode.Pipeline.required "images" postObjectRelationshipsImagesDecoder
        |> Json.Decode.Pipeline.required "media" postObjectRelationshipsMediaDecoder


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


postObjectRelationshipsImagesDecoder : Json.Decode.Decoder PostObjectRelationshipsImages
postObjectRelationshipsImagesDecoder =
    Json.Decode.succeed PostObjectRelationshipsImages
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list idAndTypeDecoder)


postObjectRelationshipsMediaDecoder : Json.Decode.Decoder PostObjectRelationshipsMedia
postObjectRelationshipsMediaDecoder =
    Json.Decode.succeed PostObjectRelationshipsMedia
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list idAndTypeDecoder)


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
        |> Json.Decode.Pipeline.required "access_rules" postMemberRelationshipsAccessRulesDecoder
        |> Json.Decode.Pipeline.required "images" postMemberRelationshipsImagesDecoder
        |> Json.Decode.Pipeline.required "media" postMemberRelationshipsMediaDecoder


postMemberRelationshipsAccessRulesDecoder : Json.Decode.Decoder PostMemberRelationshipsAccessRules
postMemberRelationshipsAccessRulesDecoder =
    Json.Decode.succeed PostMemberRelationshipsAccessRules
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list idAndTypeDecoder)


postMemberRelationshipsImagesDecoder : Json.Decode.Decoder PostMemberRelationshipsImages
postMemberRelationshipsImagesDecoder =
    Json.Decode.succeed PostMemberRelationshipsImages
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list idAndTypeDecoder)


postMemberRelationshipsMediaDecoder : Json.Decode.Decoder PostMemberRelationshipsMedia
postMemberRelationshipsMediaDecoder =
    Json.Decode.succeed PostMemberRelationshipsMedia
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list idAndTypeDecoder)


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
        |> Json.Decode.Pipeline.required "access_rules" postEntityRelationshipsAccessRulesDecoder


postEntityRelationshipsAccessRulesDecoder : Json.Decode.Decoder PostEntityRelationshipsAccessRules
postEntityRelationshipsAccessRulesDecoder =
    Json.Decode.succeed PostEntityRelationshipsAccessRules
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list idAndTypeDecoder)


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


progressDecoder : Json.Decode.Decoder Progress
progressDecoder =
    Json.Decode.succeed Progress
        |> Json.Decode.Pipeline.required "is_watched" Json.Decode.bool
        |> Json.Decode.Pipeline.required "watch_state" Json.Decode.string


postThingRelationshipsDecoder : Json.Decode.Decoder PostThingRelationships
postThingRelationshipsDecoder =
    Json.Decode.succeed PostThingRelationships
        |> Json.Decode.Pipeline.required "access_rules" postThingRelationshipsAccessRulesDecoder
        |> Json.Decode.Pipeline.required "attachments_media" postThingRelationshipsAttachmentsMediaDecoder
        |> Json.Decode.Pipeline.required "audio" postThingRelationshipsAudioDecoder
        |> Json.Decode.Pipeline.required "images" postThingRelationshipsImagesDecoder
        |> Json.Decode.Pipeline.required "media" postThingRelationshipsMediaDecoder


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


postThingRelationshipsImagesDecoder : Json.Decode.Decoder PostThingRelationshipsImages
postThingRelationshipsImagesDecoder =
    Json.Decode.succeed PostThingRelationshipsImages
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list idAndTypeDecoder)


postThingRelationshipsMediaDecoder : Json.Decode.Decoder PostThingRelationshipsMedia
postThingRelationshipsMediaDecoder =
    Json.Decode.succeed PostThingRelationshipsMedia
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list idAndTypeDecoder)


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
        |> Json.Decode.Pipeline.required "access_rules" postInstanceRelationshipsAccessRulesDecoder
        |> Json.Decode.Pipeline.required "images" postInstanceRelationshipsImagesDecoder
        |> Json.Decode.Pipeline.required "media" postInstanceRelationshipsMediaDecoder


postInstanceRelationshipsAccessRulesDecoder : Json.Decode.Decoder PostInstanceRelationshipsAccessRules
postInstanceRelationshipsAccessRulesDecoder =
    Json.Decode.succeed PostInstanceRelationshipsAccessRules
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list idAndTypeDecoder)


postInstanceRelationshipsImagesDecoder : Json.Decode.Decoder PostInstanceRelationshipsImages
postInstanceRelationshipsImagesDecoder =
    Json.Decode.succeed PostInstanceRelationshipsImages
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list idAndTypeDecoder)


postInstanceRelationshipsMediaDecoder : Json.Decode.Decoder PostInstanceRelationshipsMedia
postInstanceRelationshipsMediaDecoder =
    Json.Decode.succeed PostInstanceRelationshipsMedia
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list idAndTypeDecoder)


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
        |> Json.Decode.Pipeline.required "access_rules" postConstituentRelationshipsAccessRulesDecoder
        |> Json.Decode.Pipeline.required "images" postConstituentRelationshipsImagesDecoder
        |> Json.Decode.Pipeline.required "media" postConstituentRelationshipsMediaDecoder


postConstituentRelationshipsAccessRulesDecoder : Json.Decode.Decoder PostConstituentRelationshipsAccessRules
postConstituentRelationshipsAccessRulesDecoder =
    Json.Decode.succeed PostConstituentRelationshipsAccessRules
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list idAndTypeDecoder)


postConstituentRelationshipsImagesDecoder : Json.Decode.Decoder PostConstituentRelationshipsImages
postConstituentRelationshipsImagesDecoder =
    Json.Decode.succeed PostConstituentRelationshipsImages
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list idAndTypeDecoder)


postConstituentRelationshipsMediaDecoder : Json.Decode.Decoder PostConstituentRelationshipsMedia
postConstituentRelationshipsMediaDecoder =
    Json.Decode.succeed PostConstituentRelationshipsMedia
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list idAndTypeDecoder)


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
        |> Json.Decode.Pipeline.required "access_rules" postSpecimenRelationshipsAccessRulesDecoder
        |> Json.Decode.Pipeline.required "images" postSpecimenRelationshipsImagesDecoder
        |> Json.Decode.Pipeline.required "media" postSpecimenRelationshipsMediaDecoder
        |> Json.Decode.Pipeline.required "user_defined_tags" postSpecimenRelationshipsUserDefinedTagsDecoder


postSpecimenRelationshipsAccessRulesDecoder : Json.Decode.Decoder PostSpecimenRelationshipsAccessRules
postSpecimenRelationshipsAccessRulesDecoder =
    Json.Decode.succeed PostSpecimenRelationshipsAccessRules
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list idAndTypeDecoder)


postSpecimenRelationshipsImagesDecoder : Json.Decode.Decoder PostSpecimenRelationshipsImages
postSpecimenRelationshipsImagesDecoder =
    Json.Decode.succeed PostSpecimenRelationshipsImages
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list idAndTypeDecoder)


postSpecimenRelationshipsMediaDecoder : Json.Decode.Decoder PostSpecimenRelationshipsMedia
postSpecimenRelationshipsMediaDecoder =
    Json.Decode.succeed PostSpecimenRelationshipsMedia
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list idAndTypeDecoder)


postSpecimenRelationshipsUserDefinedTagsDecoder : Json.Decode.Decoder PostSpecimenRelationshipsUserDefinedTags
postSpecimenRelationshipsUserDefinedTagsDecoder =
    Json.Decode.succeed PostSpecimenRelationshipsUserDefinedTags
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list idAndTypeDecoder)


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
        |> Json.Decode.Pipeline.required "access_rules" postGadgetRelationshipsAccessRulesDecoder
        |> Json.Decode.Pipeline.required "audio" postGadgetRelationshipsAudioDecoder
        |> Json.Decode.Pipeline.required "media" postGadgetRelationshipsMediaDecoder
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


postGadgetRelationshipsMediaDecoder : Json.Decode.Decoder PostGadgetRelationshipsMedia
postGadgetRelationshipsMediaDecoder =
    Json.Decode.succeed PostGadgetRelationshipsMedia
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list idAndTypeDecoder)


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
        |> Json.Decode.Pipeline.required "audio" postGizmoRelationshipsAudioDecoder
        |> Json.Decode.Pipeline.required "audio_preview" postGizmoRelationshipsAudioPreviewDecoder
        |> Json.Decode.Pipeline.required "images" listOfIdAndTypeDecoder
        |> Json.Decode.Pipeline.required "media" listOfIdAndTypeDecoder


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
        |> Json.Decode.Pipeline.required "audio" postPartRelationshipsAudioDecoder
        |> Json.Decode.Pipeline.required "audio_preview" postPartRelationshipsAudioPreviewDecoder
        |> Json.Decode.Pipeline.required "media" listOfIdAndTypeDecoder
        |> Json.Decode.Pipeline.required "user_defined_tags" listOfIdAndTypeDecoder


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
        |> Json.Decode.Pipeline.required "audio" postThingamajigRelationshipsAudioDecoder
        |> Json.Decode.Pipeline.required "audio_preview" postThingamajigRelationshipsAudioPreviewDecoder
        |> Json.Decode.Pipeline.required "images" listOfIdAndTypeDecoder
        |> Json.Decode.Pipeline.required "media" listOfIdAndTypeDecoder


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
