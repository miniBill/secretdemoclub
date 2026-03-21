module BackendTask.Extra exposing (profile)

import BackendTask
import BackendTask.Custom
import BackendTask.Do as Do
import FatalError exposing (FatalError)
import Json.Decode
import Json.Encode


profile : String -> BackendTask.BackendTask FatalError a -> BackendTask.BackendTask FatalError a
profile label task =
    Do.allowFatal
        (BackendTask.Custom.run "profile" (Json.Encode.string label) (Json.Decode.succeed ()))
    <| \_ ->
    Do.do task <| \r ->
    Do.allowFatal
        (BackendTask.Custom.run "profileEnd" (Json.Encode.string label) (Json.Decode.succeed ()))
    <|
        \_ -> BackendTask.succeed r
