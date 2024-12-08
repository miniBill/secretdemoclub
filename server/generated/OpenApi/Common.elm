module OpenApi.Common exposing
    ( Nullable(..)
    , decodeOptionalField, jsonDecodeAndMap
    , Error(..), bytesResolverCustom, expectBytesCustom, expectJsonCustom, expectStringCustom, jsonResolverCustom
    , stringResolverCustom
    , decodeStringDateTime, decodeStringUri, encodeStringDateTime, encodeStringUri, toParamStringStringDateTime
    , toParamStringStringUri
    )

{-|


## Types

@docs Nullable


## Decoders

@docs decodeOptionalField, jsonDecodeAndMap


## Http

@docs Error, bytesResolverCustom, expectBytesCustom, expectJsonCustom, expectStringCustom, jsonResolverCustom
@docs stringResolverCustom


## Other

@docs decodeStringDateTime, decodeStringUri, encodeStringDateTime, encodeStringUri, toParamStringStringDateTime
@docs toParamStringStringUri

-}

import Bytes
import Bytes.Decode
import Dict
import Http
import Json.Decode
import Json.Encode
import Parser.Advanced exposing ((|.), (|=))
import Rfc3339
import Time
import Url


type Error err body
    = BadUrl String
    | Timeout
    | NetworkError
    | KnownBadStatus Int err
    | UnknownBadStatus Http.Metadata body
    | BadErrorBody Http.Metadata body
    | BadBody Http.Metadata body


type Nullable value
    = Null
    | Present value


bytesResolverCustom :
    Dict.Dict String (Json.Decode.Decoder err)
    -> Http.Resolver (Error err Bytes.Bytes) Bytes.Bytes
bytesResolverCustom errorDecoders =
    Http.bytesResolver
        (\bytesResolverUnpack ->
            case bytesResolverUnpack of
                Http.BadUrl_ stringString ->
                    Result.Err (BadUrl stringString)

                Http.Timeout_ ->
                    Result.Err Timeout

                Http.NetworkError_ ->
                    Result.Err NetworkError

                Http.BadStatus_ httpMetadata body ->
                    case
                        Dict.get
                            (String.fromInt httpMetadata.statusCode)
                            errorDecoders
                    of
                        Maybe.Just a ->
                            case
                                Json.Decode.decodeString
                                    a
                                    (Maybe.withDefault
                                        ""
                                        (Bytes.Decode.decode
                                            (Bytes.Decode.string
                                                (Bytes.width body)
                                            )
                                            body
                                        )
                                    )
                            of
                                Result.Ok value ->
                                    Result.Err
                                        (KnownBadStatus
                                            httpMetadata.statusCode
                                            value
                                        )

                                Result.Err error ->
                                    Result.Err (BadErrorBody httpMetadata body)

                        Maybe.Nothing ->
                            Result.Err (UnknownBadStatus httpMetadata body)

                Http.GoodStatus_ httpMetadata body ->
                    Result.Ok body
        )


{-| Decode an optional field

    decodeString (decodeOptionalField "x" int) "{ "x": 3 }"
    --> Ok (Just 3)

    decodeString (decodeOptionalField "x" int) "{ "x": true }"
    --> Err ...

    decodeString (decodeOptionalField "x" int) "{ "y": 4 }"
    --> Ok Nothing

-}
decodeOptionalField : String -> Json.Decode.Decoder t -> Json.Decode.Decoder (Maybe t)
decodeOptionalField key fieldDecoder =
    Json.Decode.andThen
        (\andThenUnpack ->
            if andThenUnpack then
                Json.Decode.field
                    key
                    (Json.Decode.oneOf
                        [ Json.Decode.map Just fieldDecoder
                        , Json.Decode.null Nothing
                        ]
                    )

            else
                Json.Decode.succeed Nothing
        )
        (Json.Decode.oneOf
            [ Json.Decode.map
                (\_ -> True)
                (Json.Decode.field key Json.Decode.value)
            , Json.Decode.succeed False
            ]
        )


decodeStringDateTime : Json.Decode.Decoder Time.Posix
decodeStringDateTime =
    Json.Decode.andThen
        (\andThenUnpack ->
            case
                Parser.Advanced.run Rfc3339.dateTimeOffsetParser andThenUnpack
            of
                Result.Ok value ->
                    Json.Decode.succeed value.instant

                Result.Err error ->
                    Json.Decode.fail "Invalid RFC-3339 date-time"
        )
        Json.Decode.string


decodeStringUri : Json.Decode.Decoder Url.Url
decodeStringUri =
    Json.Decode.andThen
        (\andThenUnpack ->
            case Url.fromString andThenUnpack of
                Maybe.Just a ->
                    Json.Decode.succeed a

                Maybe.Nothing ->
                    Json.Decode.fail (andThenUnpack ++ " is not a valid URL")
        )
        Json.Decode.string


encodeStringDateTime : Time.Posix -> Json.Encode.Value
encodeStringDateTime value =
    Json.Encode.string
        (Rfc3339.toString
            (Rfc3339.DateTimeOffset
                { instant = value, offset = { hour = 0, minute = 0 } }
            )
        )


encodeStringUri : Url.Url -> Json.Encode.Value
encodeStringUri value =
    Json.Encode.string (Url.toString value)


expectBytesCustom :
    (Result (Error err Bytes.Bytes) Bytes.Bytes -> msg)
    -> Dict.Dict String (Json.Decode.Decoder err)
    -> Http.Expect msg
expectBytesCustom toMsg errorDecoders =
    Http.expectBytesResponse
        toMsg
        (\expectBytesResponseUnpack ->
            case expectBytesResponseUnpack of
                Http.BadUrl_ stringString ->
                    Result.Err (BadUrl stringString)

                Http.Timeout_ ->
                    Result.Err Timeout

                Http.NetworkError_ ->
                    Result.Err NetworkError

                Http.BadStatus_ httpMetadata body ->
                    case
                        Dict.get
                            (String.fromInt httpMetadata.statusCode)
                            errorDecoders
                    of
                        Maybe.Just a ->
                            case
                                Json.Decode.decodeString
                                    a
                                    (Maybe.withDefault
                                        ""
                                        (Bytes.Decode.decode
                                            (Bytes.Decode.string
                                                (Bytes.width body)
                                            )
                                            body
                                        )
                                    )
                            of
                                Result.Ok value ->
                                    Result.Err
                                        (KnownBadStatus
                                            httpMetadata.statusCode
                                            value
                                        )

                                Result.Err error ->
                                    Result.Err (BadErrorBody httpMetadata body)

                        Maybe.Nothing ->
                            Result.Err (UnknownBadStatus httpMetadata body)

                Http.GoodStatus_ httpMetadata body ->
                    Result.Ok body
        )


expectJsonCustom :
    (Result (Error err String) success -> msg)
    -> Dict.Dict String (Json.Decode.Decoder err)
    -> Json.Decode.Decoder success
    -> Http.Expect msg
expectJsonCustom toMsg errorDecoders successDecoder =
    Http.expectStringResponse
        toMsg
        (\expectStringResponseUnpack ->
            case expectStringResponseUnpack of
                Http.BadUrl_ stringString ->
                    Result.Err (BadUrl stringString)

                Http.Timeout_ ->
                    Result.Err Timeout

                Http.NetworkError_ ->
                    Result.Err NetworkError

                Http.BadStatus_ httpMetadata body ->
                    case
                        Dict.get
                            (String.fromInt httpMetadata.statusCode)
                            errorDecoders
                    of
                        Maybe.Just a ->
                            case Json.Decode.decodeString a body of
                                Result.Ok value ->
                                    Result.Err
                                        (KnownBadStatus
                                            httpMetadata.statusCode
                                            value
                                        )

                                Result.Err error ->
                                    Result.Err (BadErrorBody httpMetadata body)

                        Maybe.Nothing ->
                            Result.Err (UnknownBadStatus httpMetadata body)

                Http.GoodStatus_ httpMetadata body ->
                    case Json.Decode.decodeString successDecoder body of
                        Result.Ok value ->
                            Result.Ok value

                        Result.Err error ->
                            Result.Err (BadBody httpMetadata body)
        )


expectStringCustom :
    (Result (Error err String) String -> msg)
    -> Dict.Dict String (Json.Decode.Decoder err)
    -> Http.Expect msg
expectStringCustom toMsg errorDecoders =
    Http.expectStringResponse
        toMsg
        (\expectStringResponseUnpack ->
            case expectStringResponseUnpack of
                Http.BadUrl_ stringString ->
                    Result.Err (BadUrl stringString)

                Http.Timeout_ ->
                    Result.Err Timeout

                Http.NetworkError_ ->
                    Result.Err NetworkError

                Http.BadStatus_ httpMetadata body ->
                    case
                        Dict.get
                            (String.fromInt httpMetadata.statusCode)
                            errorDecoders
                    of
                        Maybe.Just a ->
                            case Json.Decode.decodeString a body of
                                Result.Ok value ->
                                    Result.Err
                                        (KnownBadStatus
                                            httpMetadata.statusCode
                                            value
                                        )

                                Result.Err error ->
                                    Result.Err (BadErrorBody httpMetadata body)

                        Maybe.Nothing ->
                            Result.Err (UnknownBadStatus httpMetadata body)

                Http.GoodStatus_ httpMetadata body ->
                    Result.Ok body
        )


{-| Chain JSON decoders, when `Json.Decode.map8` isn't enough.
-}
jsonDecodeAndMap :
    Json.Decode.Decoder a
    -> Json.Decode.Decoder (a -> value)
    -> Json.Decode.Decoder value
jsonDecodeAndMap =
    Json.Decode.map2 (|>)


jsonResolverCustom :
    Dict.Dict String (Json.Decode.Decoder err)
    -> Json.Decode.Decoder success
    -> Http.Resolver (Error err String) success
jsonResolverCustom errorDecoders successDecoder =
    Http.stringResolver
        (\stringResolverUnpack ->
            case stringResolverUnpack of
                Http.BadUrl_ stringString ->
                    Result.Err (BadUrl stringString)

                Http.Timeout_ ->
                    Result.Err Timeout

                Http.NetworkError_ ->
                    Result.Err NetworkError

                Http.BadStatus_ httpMetadata body ->
                    case
                        Dict.get
                            (String.fromInt httpMetadata.statusCode)
                            errorDecoders
                    of
                        Maybe.Just a ->
                            case Json.Decode.decodeString a body of
                                Result.Ok value ->
                                    Result.Err
                                        (KnownBadStatus
                                            httpMetadata.statusCode
                                            value
                                        )

                                Result.Err error ->
                                    Result.Err (BadErrorBody httpMetadata body)

                        Maybe.Nothing ->
                            Result.Err (UnknownBadStatus httpMetadata body)

                Http.GoodStatus_ httpMetadata body ->
                    case Json.Decode.decodeString successDecoder body of
                        Result.Ok value ->
                            Result.Ok value

                        Result.Err error ->
                            Result.Err (BadBody httpMetadata body)
        )


stringResolverCustom :
    Dict.Dict String (Json.Decode.Decoder err)
    -> Http.Resolver (Error err String) String
stringResolverCustom errorDecoders =
    Http.stringResolver
        (\stringResolverUnpack ->
            case stringResolverUnpack of
                Http.BadUrl_ stringString ->
                    Result.Err (BadUrl stringString)

                Http.Timeout_ ->
                    Result.Err Timeout

                Http.NetworkError_ ->
                    Result.Err NetworkError

                Http.BadStatus_ httpMetadata body ->
                    case
                        Dict.get
                            (String.fromInt httpMetadata.statusCode)
                            errorDecoders
                    of
                        Maybe.Just a ->
                            case Json.Decode.decodeString a body of
                                Result.Ok value ->
                                    Result.Err
                                        (KnownBadStatus
                                            httpMetadata.statusCode
                                            value
                                        )

                                Result.Err error ->
                                    Result.Err (BadErrorBody httpMetadata body)

                        Maybe.Nothing ->
                            Result.Err (UnknownBadStatus httpMetadata body)

                Http.GoodStatus_ httpMetadata body ->
                    Result.Ok body
        )


toParamStringStringDateTime : Time.Posix -> String
toParamStringStringDateTime value =
    Rfc3339.toString
        (Rfc3339.DateTimeOffset
            { instant = value, offset = { hour = 0, minute = 0 } }
        )


toParamStringStringUri : Url.Url -> String
toParamStringStringUri =
    Url.toString
