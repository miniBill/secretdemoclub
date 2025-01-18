module Main exposing (run)

import Api
import BackendTask exposing (BackendTask)
import BackendTask.Do as Do
import BackendTask.Env as Env
import BackendTask.Http as Http
import Cli.Option as Option
import Cli.OptionsParser as OptionsParser
import Cli.Program as Program
import FatalError exposing (FatalError)
import List.Extra
import Pages.Script as Script exposing (Script)
import Rss exposing (Title(..))
import Rss.Parser
import SHA256
import Spinner.Reader
import Time
import Url


type alias Config =
    { outputDir : String
    , workDir : Maybe String
    , force : Bool
    , parallel : Int
    }


run : Script
run =
    Script.withCliOptions programConfig task


programConfig : Program.Config Config
programConfig =
    Program.config
        |> Program.add
            (OptionsParser.build Config
                |> OptionsParser.with (Option.requiredKeywordArg "output-dir")
                |> OptionsParser.with (Option.optionalKeywordArg "work-dir")
                |> OptionsParser.with (Option.flag "force")
                |> OptionsParser.with
                    (Option.optionalKeywordArg "parallel"
                        |> Option.withDefault "10"
                        |> Option.validateMap
                            (\i ->
                                i
                                    |> String.toInt
                                    |> Result.fromMaybe "Invalid number "
                            )
                    )
                |> OptionsParser.withDoc """

  options:
    --output-dir  The directory to output to.
    --work-dir    The directory to put intermediate files in. Defaults to `work`.
    --force       ⚠️ Rewrite existing files ⚠️.
    --parallel    How many files to download in parallel.
"""
            )


type alias Environment =
    { cookie : String
    , rssUrl : String
    }


task : Config -> BackendTask FatalError ()
task config =
    Spinner.Reader.init "Getting required secrets from the environment"
        (BackendTask.map2 Environment
            (Env.expect "cookie")
            (Env.expect "rssUrl")
            |> BackendTask.allowFatal
        )
        |> Spinner.Reader.withStep "Creating output folder"
            (\_ _ ->
                let
                    workDir : String
                    workDir =
                        config.workDir |> Maybe.withDefault "work"
                in
                Script.exec "mkdir" [ "-p", workDir ]
            )
        |> Spinner.Reader.withFatalStep "Getting posts from the Patreon API"
            (\env _ ->
                Api.getPosts env
            )
        |> Spinner.Reader.withFatalStep "Downloading RSS feed"
            (\{ rssUrl } _ ->
                Http.get rssUrl Http.expectString
            )
        |> Spinner.Reader.withStep "Writing output"
            (\_ xml ->
                case Rss.Parser.parse xml of
                    Err e ->
                        let
                            message : String
                            message =
                                "Could not parse RSS: "
                                    ++ Rss.Parser.errorToString e.error
                                    ++ "\n  at "
                                    ++ String.join " > " e.path
                        in
                        BackendTask.fail (FatalError.fromString message)

                    Ok posts ->
                        posts
                            |> List.map (writePost config)
                            |> List.Extra.greedyGroupsOf config.parallel
                            |> List.map BackendTask.combine
                            |> BackendTask.sequence
                            |> BackendTask.map (\_ -> ())
            )
        |> Spinner.Reader.runSteps


writePost : Config -> Rss.Post -> BackendTask FatalError ()
writePost config post =
    let
        workDir : String
        workDir =
            config.workDir |> Maybe.withDefault "work"

        target : String
        target =
            [ workDir
            , "/"
            , Time.toYear Time.utc post.pubDate
                |> String.fromInt
            , Time.toMonth Time.utc post.pubDate
                |> monthToNumber
                |> String.fromInt
                |> String.padLeft 2 '0'
            , Time.toDay Time.utc post.pubDate
                |> String.fromInt
                |> String.padLeft 2 '0'
            , " - "
            , post.originalTitle
                |> String.replace "/" "_"
            , ".md"
            ]
                |> String.concat
    in
    Do.glob target <| \existing ->
    if not config.force && not (List.isEmpty existing) then
        BackendTask.succeed ()

    else
        Do.do (cache config post.image) <| \image ->
        Do.do (cache config post.mediaUrl) <| \media ->
        let
            body : String
            body =
                [ "Title: " ++ Rss.titleToString post.title
                , "Category: " ++ category
                , "Date: " ++ String.fromInt (Time.posixToMillis post.pubDate)
                , "Image: " ++ image
                , "Link: " ++ post.link
                , "Media: " ++ media
                ]
                    |> String.join "\n"

            category : String
            category =
                case post.title of
                    Demo Nothing _ ->
                        "Demo"

                    Demo (Just n) _ ->
                        "Demo " ++ String.fromFloat n

                    VoiceMemo _ ->
                        "Voice memo"

                    BonusDemo _ ->
                        "Bonus demo"

                    SongIdea _ ->
                        "Song idea"

                    Podcast Nothing _ ->
                        "Podcast"

                    Podcast (Just n) _ ->
                        "Podcast " ++ String.fromInt n

                    AnIdeaADay (Err m) _ ->
                        "An idea a day " ++ m

                    AnIdeaADay (Ok o) _ ->
                        "An idea a day " ++ String.fromInt o

                    FirstDraftFebruary Nothing _ ->
                        "First draft February"

                    FirstDraftFebruary (Just n) _ ->
                        "First draft February " ++ String.fromInt n

                    AudioDiary Nothing _ ->
                        "Audio diary"

                    AudioDiary (Just n) _ ->
                        "Audio diary " ++ n

                    Other _ ->
                        "Other"
        in
        Script.writeFile
            { path = target
            , body = body
            }
            |> BackendTask.allowFatal


cache : Config -> String -> BackendTask FatalError String
cache config urlString =
    let
        workDir : String
        workDir =
            config.workDir |> Maybe.withDefault "work"
    in
    case Url.fromString urlString of
        Nothing ->
            BackendTask.fail (FatalError.fromString ("Invalid URL: " ++ urlString))

        Just url ->
            let
                clean : String
                clean =
                    Url.toString
                        { url
                            | query = Nothing
                        }

                extension : String
                extension =
                    url.path
                        |> String.split "."
                        |> List.reverse
                        |> List.head
                        |> Maybe.withDefault "???"

                hash : String
                hash =
                    SHA256.fromString clean
                        |> SHA256.toHex

                filename : String
                filename =
                    hash ++ "." ++ extension

                target : String
                target =
                    workDir ++ "/" ++ filename
            in
            Do.glob target <| \existing ->
            if not (List.isEmpty existing) then
                BackendTask.succeed filename

            else
                let
                    opts : List String
                    opts =
                        [ urlString, "-s", "-o", target ]
                in
                Do.log (String.join " " ("curl" :: opts)) <| \_ ->
                Do.exec "curl" opts <| \_ ->
                BackendTask.succeed filename


monthToNumber : Time.Month -> Int
monthToNumber month =
    case month of
        Time.Jan ->
            1

        Time.Feb ->
            2

        Time.Mar ->
            3

        Time.Apr ->
            4

        Time.May ->
            5

        Time.Jun ->
            6

        Time.Jul ->
            7

        Time.Aug ->
            8

        Time.Sep ->
            9

        Time.Oct ->
            10

        Time.Nov ->
            11

        Time.Dec ->
            12
