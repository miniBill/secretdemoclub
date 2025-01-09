module Main exposing (run)

import BackendTask exposing (BackendTask)
import BackendTask.Do as Do
import FatalError exposing (FatalError)
import Pages.Script as Script exposing (Script)


run : Script
run =
    Script.withoutCliOptions task


task : BackendTask FatalError ()
task =
    Do.env "clientId" <| \clientId ->
    Do.env "clientSecret" <| \clientSecret ->
    Do.env "orlaCampaignId" <| \orlaCampaignId ->
    BackendTask.fail (FatalError.fromString "TODO")
