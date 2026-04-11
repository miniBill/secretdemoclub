module DecodeComplete.Extra exposing (omissibleMaybe, omissibleNullableMaybe)

import DecodeComplete
import Json.Decode exposing (Decoder)


omissibleMaybe :
    String
    -> Decoder a
    -> DecodeComplete.ObjectDecoder (Maybe a -> b)
    -> DecodeComplete.ObjectDecoder b
omissibleMaybe field decoder =
    DecodeComplete.omissible field (Json.Decode.map Just decoder) Nothing


omissibleNullableMaybe :
    String
    -> Json.Decode.Decoder a
    -> DecodeComplete.ObjectDecoder (Maybe a -> b)
    -> DecodeComplete.ObjectDecoder b
omissibleNullableMaybe field decoder =
    DecodeComplete.omissible field (Json.Decode.nullable decoder) Nothing
