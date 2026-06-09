module Api exposing (AccessRule, AudioLinks, AudioRelationships, AverageColorsOfCorners, Embed, IdAndType, Image, ImageColors, ImageUrls, Post, Relationships, getPosts)

import BackendTask exposing (BackendTask)
import BackendTask.Do as Do
import BackendTask.File as File
import BackendTask.Http as Http
import DecodeComplete
import DecodeComplete.Extra
import FatalError exposing (FatalError)
import Json.Decode
import OpenApi.Common
import Pages.Script as Script
import Parser.Advanced
import PatreonInternalApi.Api
import PatreonInternalApi.Json
import PatreonInternalApi.Types as ApiTypes
import Result.Extra
import Rfc3339
import SHA256
import SeqDict exposing (SeqDict)
import Time
import Url exposing (Url)


rawPostToPost :
    Maybe (List ApiTypes.Included)
    -> ApiTypes.Post
    -> Result FatalError Post
rawPostToPost included rawPost =
    let
        includedDict : SeqDict ApiTypes.IdAndType ApiTypes.Included
        includedDict =
            List.foldl
                (\e a ->
                    SeqDict.insert (includedToIdAndType e) e a
                )
                SeqDict.empty
                (Maybe.withDefault [] included)
    in
    Result.map
        (\relationships ->
            { id = rawPost.id
            , type_ = rawPost.type_
            , attributes = rawPost.attributes
            , relationships = relationships
            }
        )
        (rawRelationshipsToRelationships includedDict rawPost.relationships)
        |> Result.mapError (\str -> FatalError.fromString ("While converting post " ++ rawPost.id ++ ", " ++ str))


includedToIdAndType : ApiTypes.Included -> ApiTypes.IdAndType
includedToIdAndType included =
    case included of
        ApiTypes.IncludedContentUnlockOption_Or_IncludedMedia_Or_IncludedPostAccessRule_Or_IncludedPostTag_Or_IncludedProductVariant_Or_IncludedReward__IncludedContentUnlockOption contentUnlockOption ->
            contentUnlockOptionToIdAndType contentUnlockOption

        ApiTypes.IncludedContentUnlockOption_Or_IncludedMedia_Or_IncludedPostAccessRule_Or_IncludedPostTag_Or_IncludedProductVariant_Or_IncludedReward__IncludedMedia media ->
            { id = media.id, type_ = media.type_ }

        ApiTypes.IncludedContentUnlockOption_Or_IncludedMedia_Or_IncludedPostAccessRule_Or_IncludedPostTag_Or_IncludedProductVariant_Or_IncludedReward__IncludedPostAccessRule postAccessRule ->
            { id = postAccessRule.id, type_ = postAccessRule.type_ }

        ApiTypes.IncludedContentUnlockOption_Or_IncludedMedia_Or_IncludedPostAccessRule_Or_IncludedPostTag_Or_IncludedProductVariant_Or_IncludedReward__IncludedPostTag postTag ->
            { id = postTag.id, type_ = postTag.type_ }

        ApiTypes.IncludedContentUnlockOption_Or_IncludedMedia_Or_IncludedPostAccessRule_Or_IncludedPostTag_Or_IncludedProductVariant_Or_IncludedReward__IncludedProductVariant productVariant ->
            { id = productVariant.id, type_ = productVariant.type_ }

        ApiTypes.IncludedContentUnlockOption_Or_IncludedMedia_Or_IncludedPostAccessRule_Or_IncludedPostTag_Or_IncludedProductVariant_Or_IncludedReward__IncludedReward reward ->
            { id = reward.id, type_ = reward.type_ }


contentUnlockOptionToIdAndType : ApiTypes.IncludedContentUnlockOption -> ApiTypes.IdAndType
contentUnlockOptionToIdAndType contentUnlockOption =
    case contentUnlockOption of
        ApiTypes.UnlockOptionMinCentsPledged_Or_UnlockOptionOneTimePayment_Or_UnlockOptionPatron_Or_UnlockOptionTier__UnlockOptionMinCentsPledged v ->
            v

        ApiTypes.UnlockOptionMinCentsPledged_Or_UnlockOptionOneTimePayment_Or_UnlockOptionPatron_Or_UnlockOptionTier__UnlockOptionOneTimePayment v ->
            v

        ApiTypes.UnlockOptionMinCentsPledged_Or_UnlockOptionOneTimePayment_Or_UnlockOptionPatron_Or_UnlockOptionTier__UnlockOptionPatron v ->
            v

        ApiTypes.UnlockOptionMinCentsPledged_Or_UnlockOptionOneTimePayment_Or_UnlockOptionPatron_Or_UnlockOptionTier__UnlockOptionTier v ->
            v


rawRelationshipsToRelationships : SeqDict IdAndType ApiTypes.Included -> ApiTypes.PostRelationships -> Result String Relationships
rawRelationshipsToRelationships included rawRelationships =
    let
        find : IdAndType -> (ApiTypes.Included -> Result String value) -> Result String value
        find key unpacker =
            case SeqDict.get key included of
                Nothing ->
                    let
                        keyToString : IdAndType -> String
                        keyToString k =
                            k.type_ ++ ":" ++ k.id
                    in
                    Err ("Could not find " ++ keyToString key ++ " in the included relationships, available keys:\n - " ++ String.join "\n - " (List.map keyToString (SeqDict.keys included)))

                Just value ->
                    unpacker value

        asAccessRule : ApiTypes.Included -> Result String AccessRule
        asAccessRule value =
            case value of
                ApiTypes.IncludedContentUnlockOption_Or_IncludedMedia_Or_IncludedPostAccessRule_Or_IncludedPostTag_Or_IncludedProductVariant_Or_IncludedReward__IncludedPostAccessRule postAccessRule ->
                    case listOfIdAndTypeToList postAccessRule.relationships.tier of
                        [] ->
                            { accessRuleType = postAccessRule.attributes.access_rule_type
                            , amountCents = postAccessRule.attributes.amount_cents
                            , currency = postAccessRule.attributes.currency
                            , postCount = postAccessRule.attributes.post_count
                            , reward = Nothing
                            }
                                |> Ok

                        [ tier ] ->
                            find tier asReward
                                |> Result.map
                                    (\reward ->
                                        { accessRuleType = postAccessRule.attributes.access_rule_type
                                        , amountCents = postAccessRule.attributes.amount_cents
                                        , currency = postAccessRule.attributes.currency
                                        , postCount = postAccessRule.attributes.post_count
                                        , reward = Just reward.attributes
                                        }
                                    )

                        _ ->
                            Err "Multiple tiers"

                _ ->
                    Err "Value is not a valid access rule"

        asReward : ApiTypes.Included -> Result String ApiTypes.IncludedReward
        asReward value =
            case value of
                ApiTypes.IncludedContentUnlockOption_Or_IncludedMedia_Or_IncludedPostAccessRule_Or_IncludedPostTag_Or_IncludedProductVariant_Or_IncludedReward__IncludedReward reward ->
                    Ok reward

                _ ->
                    Err "Value is not a valid reward"

        asMedia : ApiTypes.Included -> Result String ApiTypes.IncludedMedia
        asMedia value =
            case value of
                ApiTypes.IncludedContentUnlockOption_Or_IncludedMedia_Or_IncludedPostAccessRule_Or_IncludedPostTag_Or_IncludedProductVariant_Or_IncludedReward__IncludedMedia media ->
                    Ok media

                _ ->
                    Err "Value is not a valid media"

        asTag : ApiTypes.Included -> Result String ApiTypes.IncludedPostTag
        asTag value =
            case value of
                ApiTypes.IncludedContentUnlockOption_Or_IncludedMedia_Or_IncludedPostAccessRule_Or_IncludedPostTag_Or_IncludedProductVariant_Or_IncludedReward__IncludedPostTag media ->
                    Ok media

                _ ->
                    Err "Value is not a valid post tag"

        asContentUnlockOption : ApiTypes.Included -> Result String ApiTypes.IncludedContentUnlockOption
        asContentUnlockOption value =
            case value of
                ApiTypes.IncludedContentUnlockOption_Or_IncludedMedia_Or_IncludedPostAccessRule_Or_IncludedPostTag_Or_IncludedProductVariant_Or_IncludedReward__IncludedContentUnlockOption option ->
                    Ok option

                _ ->
                    Err "Value is not a valid post tag"

        findAll :
            ApiTypes.ListOfIdAndType
            -> (ApiTypes.Included -> Result String value)
            -> Result String (List value -> b)
            -> Result String b
        findAll prop unpacker k =
            k
                |> Result.Extra.andMap
                    (prop
                        |> listOfIdAndTypeToList
                        |> Result.Extra.combineMap (\item -> find item unpacker)
                    )

        findAllMaybe :
            Maybe ApiTypes.ListOfIdAndType
            -> (ApiTypes.Included -> Result String value)
            -> Result String (List value -> b)
            -> Result String b
        findAllMaybe prop unpacker k =
            findAll
                (Maybe.withDefault
                    (ApiTypes.EmptyListOfIdAndType_Or_NonEmptyListOfIdAndType_Or_SingleIdAndType__EmptyListOfIdAndType { data = () })
                    prop
                )
                unpacker
                k
    in
    Ok Relationships
        |> findAll rawRelationships.access_rules asAccessRule
        |> findAllMaybe rawRelationships.attachments_media asMedia
        |> findAllMaybe rawRelationships.audio asMedia
        |> findAllMaybe rawRelationships.audio_preview asMedia
        |> findAllMaybe rawRelationships.images asMedia
        |> findAllMaybe rawRelationships.video asMedia
        |> findAllMaybe rawRelationships.media asMedia
        |> findAllMaybe rawRelationships.user_defined_tags asTag
        |> findAllMaybe rawRelationships.content_unlock_options asContentUnlockOption


listOfIdAndTypeToList : ApiTypes.ListOfIdAndType -> List IdAndType
listOfIdAndTypeToList list =
    case list of
        ApiTypes.EmptyListOfIdAndType_Or_NonEmptyListOfIdAndType_Or_SingleIdAndType__EmptyListOfIdAndType _ ->
            []

        ApiTypes.EmptyListOfIdAndType_Or_NonEmptyListOfIdAndType_Or_SingleIdAndType__NonEmptyListOfIdAndType { data } ->
            data

        ApiTypes.EmptyListOfIdAndType_Or_NonEmptyListOfIdAndType_Or_SingleIdAndType__SingleIdAndType { data } ->
            case data of
                OpenApi.Common.Present d ->
                    [ d ]

                OpenApi.Common.Null ->
                    []


type alias AccessRule =
    { accessRuleType : String
    , amountCents : OpenApi.Common.Nullable Int
    , currency : String
    , postCount : Int
    , reward : Maybe ApiTypes.RewardAttributes
    }


type alias ImageUrls =
    { default : Maybe Url
    , defaultBlurred : Maybe Url
    , defaultBlurredSmall : Maybe Url
    , defaultLarge : Maybe Url
    , defaultSmall : Url
    , original : Maybe Url
    , thumbnail : Url
    , thumbnailLarge : Maybe Url
    , thumbnailSmall : Maybe Url
    , url : Url
    }


getPosts :
    { cfg | workDir : String, cookie : Maybe String }
    -> BackendTask FatalError (List Post)
getPosts config =
    let
        decodeContent : String -> BackendTask FatalError ApiTypes.PostsPage
        decodeContent content =
            Json.Decode.decodeString PatreonInternalApi.Json.decodePostsPage content
                |> Result.mapError (Json.Decode.errorToString >> FatalError.fromString)
                |> BackendTask.fromResult

        go :
            String
            -> List (List Post)
            -> BackendTask FatalError (List Post)
        go cursor acc =
            Do.do (getFromCache config (postsUrl cursor)) <| \content ->
            Do.do (decodeContent content) <| \{ meta, data, included } ->
            case Result.Extra.combineMap (rawPostToPost included) data of
                Err e ->
                    BackendTask.fail e

                Ok nextData ->
                    let
                        finalData : List (List Post)
                        finalData =
                            nextData :: acc
                    in
                    case meta.pagination.cursors.next of
                        OpenApi.Common.Present nextCursor ->
                            go nextCursor finalData

                        OpenApi.Common.Null ->
                            (finalData
                                |> List.reverse
                                |> List.concat
                                |> List.reverse
                            )
                                |> BackendTask.succeed
    in
    go "" []


getFromCache : { cfg | workDir : String, cookie : Maybe String } -> String -> BackendTask FatalError String
getFromCache config url =
    let
        hash : String
        hash =
            SHA256.fromString url
                |> SHA256.toHex

        filename : String
        filename =
            if config.cookie == Nothing then
                "anon-" ++ hash ++ ".json"

            else
                hash ++ ".json"

        target : String
        target =
            String.join "/" [ config.workDir, "internal-api", filename ]
    in
    Do.do (File.exists target) <| \fileExists ->
    if fileExists then
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
                        [ ( "User-Agent"
                          , "Mozilla/5.0 (X11; Linux x86_64; rv:133.0) Gecko/20100101 Firefox/133.0"
                          )
                            |> Just
                        , ( "Referer", "https://www.patreon.com/c/orlagartland/posts" )
                            |> Just
                        , Maybe.map (Tuple.pair "Cookie") config.cookie
                        ]
                            |> List.filterMap identity
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


postsUrl : String -> String
postsUrl cursor =
    let
        filter :
            { accessible_by_user_id : Maybe Int
            , campaign_id : Maybe Int
            , collection_ids : Maybe (List Int)
            , contains_exclusive_posts : Maybe Bool
            , include_drops : Maybe Bool
            , include_lives : Maybe Bool
            , is_draft : Maybe Bool
            , media_types : Maybe (List ApiTypes.MediaType)
            }
        filter =
            { accessible_by_user_id = Nothing
            , campaign_id = Just 119662
            , collection_ids = Nothing
            , contains_exclusive_posts = Nothing
            , include_drops = Nothing
            , include_lives = Nothing
            , is_draft = Nothing
            , media_types = Nothing
            }

        fields_ :
            { access_rule : Maybe (List ApiTypes.AccessRuleField)
            , campaign : Maybe (List ApiTypes.CampaignField)
            , content_unlock_option : Maybe (List ApiTypes.ContentUnlockOptionField)
            , livestream : Maybe (List ApiTypes.LivestreamField)
            , media : Maybe (List ApiTypes.MediaField)
            , post : Maybe (List ApiTypes.PostField)
            , post_tag : Maybe (List ApiTypes.PostTagField)
            , productvariant : Maybe (List ApiTypes.ProductVariantField)
            , user : Maybe (List ApiTypes.UserField)
            }
        fields_ =
            { campaign = Nothing
            , livestream = Nothing
            , user = Nothing
            , post =
                [ ApiTypes.PostField__ChangeVisibilityAt
                , ApiTypes.PostField__Content
                , ApiTypes.PostField__CreatedAt
                , ApiTypes.PostField__Embed
                , ApiTypes.PostField__Image
                , ApiTypes.PostField__MetaImageUrl
                , ApiTypes.PostField__PostFile
                , ApiTypes.PostField__PostMetadata
                , ApiTypes.PostField__PublishedAt
                , ApiTypes.PostField__PatreonUrl
                , ApiTypes.PostField__PostType
                , ApiTypes.PostField__PledgeUrl
                , ApiTypes.PostField__PreviewAssetType
                , ApiTypes.PostField__Thumbnail
                , ApiTypes.PostField__ThumbnailUrl
                , ApiTypes.PostField__Title
                , ApiTypes.PostField__Url
                , ApiTypes.PostField__Video
                , ApiTypes.PostField__VideoPreview
                , ApiTypes.PostField__ContentUnlockOptions
                ]
                    |> Just
            , post_tag =
                [ ApiTypes.PostTagField__TagType
                , ApiTypes.PostTagField__Value
                ]
                    |> Just
            , access_rule =
                [ ApiTypes.AccessRuleField__AccessRuleType
                ]
                    |> Just
            , media =
                [ ApiTypes.MediaField__Id
                , ApiTypes.MediaField__ImageUrls
                , ApiTypes.MediaField__Display
                , ApiTypes.MediaField__DownloadUrl
                , ApiTypes.MediaField__Metadata
                , ApiTypes.MediaField__FileName
                ]
                    |> Just
            , productvariant =
                [ ApiTypes.ProductVariantField__PriceCents
                , ApiTypes.ProductVariantField__CurrencyCode
                , ApiTypes.ProductVariantField__CheckoutUrl
                , ApiTypes.ProductVariantField__IsHidden
                , ApiTypes.ProductVariantField__PublishedAtDatetime
                , ApiTypes.ProductVariantField__ContentType
                , ApiTypes.ProductVariantField__OrdersCount
                , ApiTypes.ProductVariantField__AccessMetadata
                ]
                    |> Just
            , content_unlock_option =
                [ ApiTypes.ContentUnlockOptionField__ContentUnlockType
                ]
                    |> Just
            }
    in
    PatreonInternalApi.Api.postsRecord
        { params =
            { include =
                Just
                    [ ApiTypes.Include__AccessRules
                    , ApiTypes.Include__AccessRulesTier
                    , ApiTypes.Include__AttachmentsMedia
                    , ApiTypes.Include__Audio
                    , ApiTypes.Include__AudioPreviewNull
                    , ApiTypes.Include__Images
                    , ApiTypes.Include__Media
                    , ApiTypes.Include__UserDefinedTags
                    , ApiTypes.Include__VideoNull
                    , ApiTypes.Include__ContentUnlockOptionsProductVariantNull
                    ]
            , fields = Just fields_
            , filter = Just filter
            , sort = Just ApiTypes.Sort__MinuspublishedAt
            , page = Just { cursor = Just cursor }
            , json_api_use_default_includes = Just False
            , json_api_version = Just "1.0"
            }
        }
        |> Tuple.first
        |> .url


type alias Post =
    { id : String
    , type_ : ApiTypes.PostType
    , attributes : ApiTypes.PostAttributes
    , relationships : Relationships
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
    { height : Maybe Int
    , largeUrl : Maybe Url
    , thumbSquareLargeUrl : Maybe Url
    , thumbSquareUrl : Maybe Url
    , thumbUrl : Maybe Url
    , url : Url
    , width : Maybe Int
    }


type alias Relationships =
    { accessRules : List AccessRule
    , attachmentsMedia : List ApiTypes.IncludedMedia
    , audio : List ApiTypes.IncludedMedia
    , audioPreview : List ApiTypes.IncludedMedia
    , images : List ApiTypes.IncludedMedia
    , video : List ApiTypes.IncludedMedia
    , media : List ApiTypes.IncludedMedia
    , userDefinedTags : List ApiTypes.IncludedPostTag
    , contentUnlockOptions : List ApiTypes.IncludedContentUnlockOption
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
