module DecodeComplete exposing
    ( ObjectDecoder, object
    , required, optional, omissible, discard, discardOptional, hardcoded
    , complete, discardRest, rest, restValues
    , andThen, fail
    )

{-| This module provides a way to decode JSON objects while making sure that all fields are handled. The interface works similar to json-decode-pipeline. For example,

    import DecodeComplete exposing (..)
    import Json.Decode as D exposing (Decoder)

    type alias User =
        { name : String
        , age : Int
        }

    userDecoder : Decoder User
    userDecoder =
        object User
            |> required "name" D.string
            |> required "age" D.int
            |> discard "email"
            |> complete

decodes JSON objects that have precisely the fields `name`, `age`, and `email` and turns it into a `User` record, discarding the email address.

The general usage is as follows: Start decoding the object with `object f`, where `f` is the function being called with the results. Then decode the individual fields with `require`, `discard`, `optional`, `omissible`, `discardOptional`. At the end, turn turn the `ObjectDecoder` into a normal `Decoder` by calling `complete` (or `discardRest` or `rest` or `restValues`).


# Starting to decode

@docs ObjectDecoder, object


# Decoding fields

@docs required, optional, omissible, discard, discardOptional, hardcoded


# Finish decoding

@docs complete, discardRest, rest, restValues


# Special needs – decoding custom types and versioned data

@docs andThen, fail

-}

import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Set exposing (Set)


{-| A decoder for JSON objects that makes sure that all fields in the JSON are handled
-}
type ObjectDecoder a
    = OD (Decoder ( Set String, a ))


{-| Start decoding a JSON object.
-}
object : a -> ObjectDecoder a
object a =
    OD (D.map2 Tuple.pair (D.dict (D.succeed ()) |> D.map (\dict -> Dict.keys dict |> Set.fromList)) (D.succeed a))


lift : (( Set String, a ) -> Decoder ( Set String, b )) -> ObjectDecoder a -> ObjectDecoder b
lift handler (OD objectDecoder) =
    OD (objectDecoder |> D.andThen handler)


{-| Decode the field given by the `String` parameter using the given (regular) `Decoder`. If the field is missing, the decoder fails.
-}
required : String -> Decoder a -> ObjectDecoder (a -> b) -> ObjectDecoder b
required field decoder =
    lift
        (\( unhandled, f ) ->
            D.field field decoder
                |> D.map (\result -> ( Set.remove field unhandled, f result ))
        )


{-| Decode the field given by the `String` parameter using the given (regular) `Decoder`. If the field is missing or the decoder fails, use the provided default value instead.

If you want to not ignore field decoding failures, use `omissible`.

-}
optional : String -> Decoder a -> a -> ObjectDecoder (a -> b) -> ObjectDecoder b
optional field decoder default =
    lift
        (\( unhandled, f ) ->
            D.maybe (D.field field decoder)
                |> D.map (Maybe.withDefault default)
                |> D.map (\result -> ( Set.remove field unhandled, f result ))
        )


{-| Decode the field given by the `String` parameter using the given (regular) `Decoder`. If the field is missing use the provided default value instead. If the field decoder fails, the object decoder will fail.

If you want to ignore field decoding failures, use `optional`.

-}
omissible : String -> Decoder a -> a -> ObjectDecoder (a -> b) -> ObjectDecoder b
omissible field decoder default =
    lift
        (\( unhandled, f ) ->
            D.maybe (D.field field D.value)
                |> D.andThen
                    (\present ->
                        case present of
                            Just _ ->
                                D.map Just (D.field field decoder)

                            Nothing ->
                                D.succeed Nothing
                    )
                |> D.map (Maybe.withDefault default)
                |> D.map (\result -> ( Set.remove field unhandled, f result ))
        )


{-| Require that a field is present, but discard its value.
-}
discard : String -> ObjectDecoder a -> ObjectDecoder a
discard field =
    lift
        (\( unhandled, a ) ->
            if Set.member field unhandled then
                D.succeed ( Set.remove field unhandled, a )

            else
                D.fail ("Missing required discarded field `" ++ field ++ "`")
        )


{-| Discard the value of a field (thus marking it as handled), but simply ignore if its not there.
-}
discardOptional : String -> ObjectDecoder a -> ObjectDecoder a
discardOptional field =
    lift (\( unhandled, a ) -> D.succeed ( Set.remove field unhandled, a ))


{-| Don’t look at the JSON, simply use the given value.
-}
hardcoded : a -> ObjectDecoder (a -> b) -> ObjectDecoder b
hardcoded a =
    lift (\( unhandled, f ) -> D.succeed ( unhandled, f a ))


{-| Close the `ObjectDecoder`, turning it into a regular `Decoder`. If unhandled fields in the JSON remain, this decoder will fail.
-}
complete : ObjectDecoder a -> Decoder a
complete (OD objectDecoder) =
    objectDecoder
        |> D.andThen
            (\( unhandled, a ) ->
                if Set.isEmpty unhandled then
                    D.succeed a

                else
                    D.fail ("The following fields where not handled: " ++ String.join ", " (Set.toList unhandled))
            )


{-| Turn the `ObjectDecoder` into a regular `Decoder`. Ignore if fields remain unhandled.

This might be useful if you only want the check that all fields are handled to occur during development. You can use `complete` in development and change it into `discardRest` without having to change anything else.

-}
discardRest : ObjectDecoder a -> Decoder a
discardRest (OD objectDecoder) =
    objectDecoder |> D.map Tuple.second


{-| Decode the remaining fields uniformly with the given `Decoder`, pass the dictionary of the results and close the `ObjectDecoder` turning it into a regular `Decoder`.
-}
rest : Decoder a -> ObjectDecoder (Dict String a -> b) -> Decoder b
rest aDecoder (OD objectDecoder) =
    objectDecoder
        |> D.andThen
            (\( unhandled, f ) ->
                Set.foldl
                    (\field acc ->
                        D.map2 (Dict.insert field) (D.field field aDecoder) acc
                    )
                    (D.succeed Dict.empty)
                    unhandled
                    |> D.map f
            )


{-| Finish up the `ObjectDecoder`, turning it into a regular decoder. Pass a dictionary of the unhandled fields (as `Decode.Value` values).
-}
restValues : ObjectDecoder (Dict String D.Value -> b) -> Decoder b
restValues =
    rest D.value


{-| Decide how to proceed based on earlier fields. This can be useful if the JSON represents a sum type or has different versions. For example

    userDecoder : Decoder ( String, Int )
    userDecoder =
        (object identity
            |> required "version" D.int
        )
            |> andThen
                (\version ->
                    case version of
                        0 ->
                            object Tuple.pair
                                |> required "name" D.string
                                |> required "age" D.int

                        1 ->
                            object Tuple.pair
                                |> required "fullName" D.string
                                |> required "age" D.int
                                |> discard "email"

                        _ ->
                            fail "unsupported version"
                )
            |> complete

first decodes the `version` field. If it is `0`, the JSON needs to have (exactly) the fields `name` and `age`. If the version is `1`, the JSON needs the fields `fullName`, `age` and `email` instead. If the version is anything else, fail.

-}
andThen : (a -> ObjectDecoder b) -> ObjectDecoder a -> ObjectDecoder b
andThen cont =
    lift
        (\( unhandled, a ) ->
            case cont a of
                OD nextDecoder ->
                    nextDecoder
                        |> D.map (\( subUnhandled, result ) -> ( Set.intersect unhandled subUnhandled, result ))
        )


{-| An `ObjectDecoder` that always fails. Can be useful in combination with `andThen`.
-}
fail : String -> ObjectDecoder a
fail message =
    OD (D.fail message)
