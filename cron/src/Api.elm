module Api exposing (getPosts)

import BackendTask exposing (BackendTask)
import BackendTask.Do as Do
import BackendTask.File as File
import BackendTask.Http as Http
import FatalError exposing (FatalError)
import Json.Decode
import Json.Decode.Pipeline
import Pages.Script as Script
import SHA256
import Url exposing (Url)
import Url.Builder


getPosts :
    { a | workDir : String, cookie : String }
    -> BackendTask FatalError (List Post)
getPosts cookie =
    getPaginated cookie postsUrl postDecoder


getPaginated :
    { cfg | workDir : String, cookie : String }
    -> (String -> String)
    -> Json.Decode.Decoder a
    -> BackendTask FatalError (List a)
getPaginated config toUrl itemDecoder =
    let
        pageDecoder : Json.Decode.Decoder { data : List a, next : Maybe String }
        pageDecoder =
            Json.Decode.map2 (\data next -> { data = data, next = next })
                (Json.Decode.field "data" (Json.Decode.list itemDecoder))
                (Json.Decode.at
                    [ "meta", "pagination", "cursors", "next" ]
                    (Json.Decode.nullable Json.Decode.string)
                )

        decodeContent : String -> BackendTask FatalError { data : List a, next : Maybe String }
        decodeContent content =
            Json.Decode.decodeString pageDecoder content
                |> Result.mapError (Json.Decode.errorToString >> FatalError.fromString)
                |> BackendTask.fromResult

        go :
            String
            -> List (List a)
            -> BackendTask FatalError (List a)
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
            Do.do (decodeContent content) <| \{ next, data } ->
            let
                nextAcc : List (List a)
                nextAcc =
                    data :: acc
            in
            case next of
                Just nextCursor ->
                    go nextCursor nextAcc

                Nothing ->
                    nextAcc
                        |> List.reverse
                        |> List.concat
                        |> List.reverse
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
    { content : Maybe String
    , createdAt : String
    , embed : Maybe Embed
    , image : Maybe Image
    , metaImageUrl : Url
    , patreonUrl : String
    , pledgeUrl : String
    , postFile : Maybe PostFile
    , postMetadata : Maybe PostMetadata
    , postType : String
    , previewAssetType : Maybe String
    , publishedAt : String
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
    = PostFileVideo PostVideo
    | PostFileImage PostImage


type alias PostVideo =
    { defaultThumbnail : Url
    , duration : Maybe Float
    , fullContentDuration : Maybe Float
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
        |> Json.Decode.Pipeline.optional "content" (Json.Decode.map Just Json.Decode.string) Nothing
        |> Json.Decode.Pipeline.required "created_at" Json.Decode.string
        |> Json.Decode.Pipeline.optional "embed" (Json.Decode.map Just embedDecoder) Nothing
        |> Json.Decode.Pipeline.optional "image" (Json.Decode.map Just imageDecoder) Nothing
        |> Json.Decode.Pipeline.required "meta_image_url" urlDecoder
        |> Json.Decode.Pipeline.required "patreon_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "pledge_url" Json.Decode.string
        |> Json.Decode.Pipeline.optional "post_file" (Json.Decode.map Just postFileDecoder) Nothing
        |> Json.Decode.Pipeline.optional "post_metadata" (Json.Decode.map Just postMetadataDecoder) Nothing
        |> Json.Decode.Pipeline.required "post_type" Json.Decode.string
        |> Json.Decode.Pipeline.optional "preview_asset_type" (Json.Decode.map Just Json.Decode.string) Nothing
        |> Json.Decode.Pipeline.required "published_at" Json.Decode.string
        |> Json.Decode.Pipeline.optional "thumbnail" (Json.Decode.map Just thumbnailDecoder) Nothing
        |> Json.Decode.Pipeline.required "title" (Json.Decode.nullable Json.Decode.string)
        |> Json.Decode.Pipeline.required "url" urlDecoder


imageDecoder : Json.Decode.Decoder Image
imageDecoder =
    Json.Decode.succeed Image
        |> Json.Decode.Pipeline.required "height" Json.Decode.int
        |> Json.Decode.Pipeline.optional "large_url" (Json.Decode.map Just urlDecoder) Nothing
        |> Json.Decode.Pipeline.optional "thumb_square_large_url" (Json.Decode.map Just urlDecoder) Nothing
        |> Json.Decode.Pipeline.optional "thumb_square_url" (Json.Decode.map Just urlDecoder) Nothing
        |> Json.Decode.Pipeline.optional "thumb_url" (Json.Decode.map Just urlDecoder) Nothing
        |> Json.Decode.Pipeline.required "url" urlDecoder
        |> Json.Decode.Pipeline.required "width" Json.Decode.int


postVideoDecoder : Json.Decode.Decoder PostVideo
postVideoDecoder =
    Json.Decode.succeed PostVideo
        |> Json.Decode.Pipeline.required "default_thumbnail" (Json.Decode.field "url" urlDecoder)
        |> Json.Decode.Pipeline.optional "duration" (Json.Decode.map Just Json.Decode.float) Nothing
        |> Json.Decode.Pipeline.optional "full_content_duration" (Json.Decode.map Just Json.Decode.float) Nothing
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
        |> Json.Decode.Pipeline.optional "description" (Json.Decode.map Just Json.Decode.string) Nothing
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
    Json.Decode.oneOf
        [ Json.Decode.map Thumbnail_Square squareThumbnailDecoder
        , Json.Decode.map Thumbnail_Gif gifThumbnailDecoder
        ]


squareThumbnailDecoder : Json.Decode.Decoder SquareThumbnail
squareThumbnailDecoder =
    Json.Decode.succeed SquareThumbnail
        |> Json.Decode.Pipeline.required "large" Json.Decode.string
        |> Json.Decode.Pipeline.required "large_2" Json.Decode.string
        |> Json.Decode.Pipeline.required "square" Json.Decode.string
        |> Json.Decode.Pipeline.required "url" urlDecoder


gifThumbnailDecoder : Json.Decode.Decoder GifThumbnail
gifThumbnailDecoder =
    Json.Decode.succeed GifThumbnail
        |> Json.Decode.Pipeline.required "gif_url" urlDecoder
        |> Json.Decode.Pipeline.required "height" Json.Decode.int
        |> Json.Decode.Pipeline.required "url" urlDecoder
        |> Json.Decode.Pipeline.required "width" Json.Decode.int


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
