module BackendTask.Extra exposing (profile, setupDebugger, triggerDebugger)

import BackendTask exposing (BackendTask)
import BackendTask.Custom
import BackendTask.Do as Do
import FatalError exposing (FatalError)
import Json.Decode
import Json.Encode


profile : String -> BackendTask.BackendTask FatalError a -> BackendTask.BackendTask FatalError a
profile label task =
    Do.allowFatal
        (BackendTask.Custom.run "profile" (Json.Encode.string label) (Json.Decode.succeed ()) |> BackendTask.quiet)
    <| \_ ->
    Do.do task <| \r ->
    Do.allowFatal
        (BackendTask.Custom.run "profileEnd" (Json.Encode.string label) (Json.Decode.succeed ()) |> BackendTask.quiet)
    <|
        \_ -> BackendTask.succeed r


setupDebugger : BackendTask FatalError ()
setupDebugger =
    BackendTask.Custom.run "setupDebugger" Json.Encode.null (Json.Decode.succeed ())
        |> BackendTask.allowFatal
        |> BackendTask.quiet


triggerDebugger : a -> a
triggerDebugger x =
    Debug.log "!!!BREAK!!!" x
