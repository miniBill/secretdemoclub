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


type alias Post =
    { attributes : Attributes
    , id : String
    , relationships : Relationships
    , type_ : String
    }


type alias Attributes =
    { content : String
    , createdAt : String
    , metaImageUrl : Url
    , patreonUrl : String
    , pledgeUrl : String
    , postType : String
    , publishedAt : String
    , title : String
    , url : Url
    , embed : Maybe Embed
    , image : Maybe Image
    , postFile : Maybe PostFile
    , postMetadata : Maybe PostMetadata
    , previewAssetType : Maybe String
    , thumbnail : Maybe Thumbnail
    }


type alias Embed =
    { description : String
    , html : Maybe String
    , provider : String
    , providerUrl : String
    , subject : String
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
    = MetadataNone
    | MetadataWithImageOrder (List String)
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


type alias Relationships =
    { accessRules : List IdAndType
    , attachmentsMedia : List IdAndType
    , audio : Maybe AudioRelationships
    , audioPreview : Maybe AudioRelationships
    , images : List IdAndType
    , media : List IdAndType
    , userDefinedTags : List IdAndType
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


postDecoder : Json.Decode.Decoder Post
postDecoder =
    Json.Decode.succeed Post
        |> Json.Decode.Pipeline.required "attributes" attributesDecoder
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "relationships" relationshipsDecoder
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


attributesDecoder : Json.Decode.Decoder Attributes
attributesDecoder =
    Json.Decode.succeed Attributes
        |> Json.Decode.Pipeline.required "content" Json.Decode.string
        |> Json.Decode.Pipeline.required "created_at" Json.Decode.string
        |> Json.Decode.Pipeline.required "meta_image_url" urlDecoder
        |> Json.Decode.Pipeline.required "patreon_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "pledge_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "post_type" Json.Decode.string
        |> Json.Decode.Pipeline.required "published_at" Json.Decode.string
        |> Json.Decode.Pipeline.required "title" Json.Decode.string
        |> Json.Decode.Pipeline.required "url" urlDecoder
        |> Json.Decode.Pipeline.optional "embed" (Json.Decode.map Just embedDecoder) Nothing
        |> Json.Decode.Pipeline.optional "image" (Json.Decode.map Just imageDecoder) Nothing
        |> Json.Decode.Pipeline.optional "post_file" (Json.Decode.map Just postFileDecoder) Nothing
        |> Json.Decode.Pipeline.optional "post_metadata" (Json.Decode.map Just postMetadataDecoder) Nothing
        |> Json.Decode.Pipeline.optional "preview_asset_type" (Json.Decode.map Just Json.Decode.string) Nothing
        |> Json.Decode.Pipeline.optional "thumbnail" (Json.Decode.map Just thumbnailDecoder) Nothing


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
        |> Json.Decode.Pipeline.required "default_thumbnail" (Json.Decode.field "url" urlDecoder)
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
        , Json.Decode.succeed MetadataNone
        ]


relationshipsDecoder : Json.Decode.Decoder Relationships
relationshipsDecoder =
    Json.Decode.succeed Relationships
        |> Json.Decode.Pipeline.required "access_rules" listOfIdAndTypeDecoder
        |> Json.Decode.Pipeline.optional "attachments_media" listOfIdAndTypeDecoder []
        |> Json.Decode.Pipeline.optional "audio"
            (Json.Decode.oneOf
                [ Json.Decode.map Just audioRelationshipsDecoder
                , Json.Decode.field "data" (Json.Decode.null Nothing)
                ]
            )
            Nothing
        |> Json.Decode.Pipeline.optional "audio_preview"
            (Json.Decode.oneOf
                [ Json.Decode.map Just audioRelationshipsDecoder
                , Json.Decode.field "data" (Json.Decode.null Nothing)
                ]
            )
            Nothing
        |> Json.Decode.Pipeline.optional "images" listOfIdAndTypeDecoder []
        |> Json.Decode.Pipeline.optional "media" listOfIdAndTypeDecoder []
        |> Json.Decode.Pipeline.optional "userDefinedTags" listOfIdAndTypeDecoder []


idAndTypeDecoder : Json.Decode.Decoder IdAndType
idAndTypeDecoder =
    Json.Decode.succeed IdAndType
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string


embedDecoder : Json.Decode.Decoder Embed
embedDecoder =
    Json.Decode.succeed Embed
        |> Json.Decode.Pipeline.required "description" Json.Decode.string
        |> Json.Decode.Pipeline.optional "html" (Json.Decode.map Just Json.Decode.string) Nothing
        |> Json.Decode.Pipeline.required "provider" Json.Decode.string
        |> Json.Decode.Pipeline.required "provider_url" Json.Decode.string
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
    Json.Decode.succeed AudioRelationships
        |> Json.Decode.Pipeline.required "data" idAndTypeDecoder
        |> Json.Decode.Pipeline.required "links" audioLinksDecoder


audioLinksDecoder : Json.Decode.Decoder AudioLinks
audioLinksDecoder =
    Json.Decode.succeed AudioLinks
        |> Json.Decode.Pipeline.required "related" Json.Decode.string


listOfIdAndTypeDecoder : Json.Decode.Decoder (List IdAndType)
listOfIdAndTypeDecoder =
    Json.Decode.succeed identity
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list idAndTypeDecoder)
