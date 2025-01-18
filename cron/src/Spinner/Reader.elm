module Spinner.Reader exposing (..)

import BackendTask exposing (BackendTask)
import FatalError exposing (FatalError)
import Pages.Script.Spinner as Spinner


type alias Steps err env a =
    Spinner.Steps err ( env, a )


init :
    String
    -> BackendTask FatalError env
    -> Steps FatalError env ()
init label task =
    Spinner.steps
        |> Spinner.withStep label
            (\() ->
                task
                    |> BackendTask.map
                        (\env ->
                            ( env, () )
                        )
            )


withStep :
    String
    -> (env -> a -> BackendTask FatalError b)
    -> Steps FatalError env a
    -> Steps FatalError env b
withStep label task =
    Spinner.withStep label
        (\( env, prev ) ->
            task env prev
                |> BackendTask.map (\next -> ( env, next ))
        )


withFatalStep :
    String
    -> (env -> a -> BackendTask { error | fatal : FatalError } b)
    -> Steps FatalError env a
    -> Steps FatalError env b
withFatalStep label task =
    Spinner.withStep label
        (\( env, prev ) ->
            task env prev
                |> BackendTask.map (\next -> ( env, next ))
                |> BackendTask.allowFatal
        )


runSteps : Steps FatalError env a -> BackendTask FatalError a
runSteps steps =
    Spinner.runSteps steps
        |> BackendTask.map Tuple.second
