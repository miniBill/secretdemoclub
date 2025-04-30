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
import Parser
import Rss exposing (Title(..))
import Rss.Parser
import SHA256
import Spinner.Reader
import Time
import Url


type alias Config =
    { outputDir : String
    , workDir : String
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
                |> OptionsParser.with (Option.optionalKeywordArg "work-dir" |> Option.withDefault "work")
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
                Do.exec "mkdir" [ "-p", config.workDir ++ "/media" ] <| \_ ->
                Do.exec "rm" [ "-rf", config.workDir ++ "/media-scratch" ] <| \_ ->
                Do.exec "mkdir" [ config.workDir ++ "/media-scratch" ] <| \_ ->
                Do.exec "mkdir" [ "-p", config.outputDir ] <| \_ ->
                Do.noop
            )
        |> Spinner.Reader.withStep "Getting posts from the Patreon API"
            (\env _ ->
                Api.getPosts { workDir = config.workDir, cookie = env.cookie }
            )
        -- |> Spinner.Reader.withFatalStep "Downloading RSS feed"
        --     (\{ rssUrl } apiPosts ->
        --         BackendTask.map
        --             (Tuple.pair apiPosts)
        --             (Http.get rssUrl Http.expectString)
        --     )
        |> Spinner.Reader.withStep "Downloading media"
            (\_ (apiPosts {- , xml -}) ->
                -- case Rss.Parser.parse xml of
                --     Err e ->
                --         let
                --             message : String
                --             message =
                --                 "Could not parse RSS: "
                --                     ++ Rss.Parser.errorToString e.error
                --                     ++ "\n  at "
                --                     ++ String.join " > " e.path
                --         in
                --         BackendTask.fail (FatalError.fromString message)
                --     Ok posts ->
                apiPosts
                    |> List.map (\post -> cachePost config post)
                    |> List.Extra.greedyGroupsOf config.parallel
                    |> List.map BackendTask.combine
                    |> BackendTask.sequence
                    |> BackendTask.map
                        (\groups ->
                            groups
                                |> List.concat
                                |> List.filterMap identity
                        )
            )
        |> Spinner.Reader.withStep "Writing posts"
            (\_ cachedPosts ->
                cachedPosts
                    |> List.map (writePost config)
                    |> List.Extra.greedyGroupsOf 10
                    |> List.map BackendTask.combine
                    |> BackendTask.sequence
                    |> BackendTask.map List.concat
            )
        |> Spinner.Reader.withStep "Writing index"
            (\_ contentAddresses ->
                let
                    path : String
                    path =
                        config.workDir ++ "/index.md"

                    body : String
                    body =
                        contentAddresses
                            |> List.map contentAddressToPath
                            |> String.join "\n"
                in
                Do.allowFatal
                    (Script.writeFile
                        { path = path
                        , body = body
                        }
                    )
                <| \_ ->
                copyToContentAddressableStorage config { path = path, extension = "md" }
            )
        |> Spinner.Reader.runSteps
        |> BackendTask.andThen (\contentAddress -> Script.log ("Index is at " ++ contentAddressToPath contentAddress))


cachePost : Config -> Api.RawPost -> BackendTask FatalError (Maybe { image : ContentAddress, media : ContentAddress, post : Api.RawPost })
cachePost config post =
    let
        _ =
            if post.id == "6184215" && False then
                Debug.log "'change'" post

            else
                post
    in
    case post.attributes.postType of
        "podcast" ->
            case
                Maybe.map2 Tuple.pair
                    (Maybe.andThen .thumbSquareUrl post.attributes.image)
                    (Maybe.andThen
                        (\postFile ->
                            case postFile of
                                Api.PostFileAudioVideo { url } ->
                                    Just url

                                Api.PostFileImage _ ->
                                    Debug.todo "branch 'PostFileImage _' not implemented"

                                Api.PostFileSize _ ->
                                    Debug.todo "branch 'PostFileSize _' not implemented"
                        )
                        post.attributes.postFile
                    )
            of
                Nothing ->
                    BackendTask.succeed Nothing

                Just ( thumbUrl, mediaUrl ) ->
                    Do.do (cache config (Url.toString thumbUrl)) <| \image ->
                    Do.do (cache config (Url.toString mediaUrl)) <| \media ->
                    BackendTask.succeed (Just { image = image, media = media, post = post })

        "livestream_youtube" ->
            BackendTask.succeed Nothing

        "text_only" ->
            BackendTask.succeed Nothing

        "image_file" ->
            BackendTask.succeed Nothing

        "link" ->
            BackendTask.succeed Nothing

        "video_embed" ->
            BackendTask.succeed Nothing

        "video_external_file" ->
            BackendTask.succeed Nothing

        "poll" ->
            BackendTask.succeed Nothing

        "livestream_crowdcast" ->
            BackendTask.succeed Nothing

        "audio_embed" ->
            BackendTask.succeed Nothing

        _ ->
            Debug.todo ("Unsupported post type: " ++ post.attributes.postType ++ " for https://www.patreon.com" ++ post.attributes.patreonUrl)


writePost : Config -> { image : ContentAddress, media : ContentAddress, post : Api.RawPost } -> BackendTask FatalError ContentAddress
writePost config { image, media, post } =
    let
        filename : String
        filename =
            [ Time.toYear Time.utc post.attributes.publishedAt
                |> String.fromInt
            , Time.toMonth Time.utc post.attributes.publishedAt
                |> monthToNumber
                |> String.fromInt
                |> String.padLeft 2 '0'
            , Time.toDay Time.utc post.attributes.publishedAt
                |> String.fromInt
                |> String.padLeft 2 '0'
            , " - "
            , post.attributes.title
                |> Maybe.withDefault ""
                |> String.replace "/" "_"
            ]
                |> String.concat

        postPath : PostPath
        postPath =
            PostPath { filename = filename, extension = "md" }

        target : String
        target =
            postPathToPath config postPath

        title : Rss.Title
        title =
            case post.attributes.title of
                Nothing ->
                    Other ""

                Just t ->
                    Parser.run Rss.Parser.titleParser t
                        |> Result.withDefault (Other t)

        body : String
        body =
            [ Just ("Title: " ++ Rss.titleToString title)
            , Just ("Category: " ++ titleToCategory title)
            , Just ("Date: " ++ String.fromInt (Time.posixToMillis post.attributes.publishedAt))
            , Just ("Image: " ++ contentAddressToPath image)
            , Just ("Link: https://www.patreon.com" ++ post.attributes.patreonUrl)
            , Just ("Media: " ++ contentAddressToPath media)
            , Maybe.map (\num -> "Number: " ++ num) (toNumber title)
            ]
                |> List.filterMap identity
                |> String.join "\n"
    in
    Do.glob target <| \existing ->
    Do.allowFatal
        (if not config.force && not (List.isEmpty existing) then
            Do.noop

         else
            Script.writeFile
                { path = target
                , body = body
                }
        )
    <| \_ ->
    copyPostToContentAddressableStorage config postPath


titleToCategory : Rss.Title -> String
titleToCategory title =
    case title of
        Demo _ _ ->
            "Demo"

        VoiceMemo _ ->
            "Voice memo"

        BonusDemo _ ->
            "Bonus demo"

        SongIdea _ ->
            "Song idea"

        Podcast _ _ ->
            "Podcast"

        AnIdeaADay _ _ ->
            "An idea a day"

        FirstDraftFebruary _ _ ->
            "First draft February"

        AudioDiary _ _ ->
            "Audio diary"

        Other _ ->
            "Other"


type ScratchPath
    = ScratchPath { filename : String, extension : String }


scratchPathToPath : Config -> ScratchPath -> String
scratchPathToPath config (ScratchPath { filename, extension }) =
    String.join "/" [ config.workDir, "media-scratch", filename ++ "." ++ extension ]


type MediaPath
    = MediaPath { filename : String, extension : String }


mediaPathToPath : Config -> MediaPath -> String
mediaPathToPath config (MediaPath { filename, extension }) =
    String.join "/" [ config.workDir, "media", filename ++ "." ++ extension ]


type PostPath
    = PostPath { filename : String, extension : String }


postPathToPath : Config -> PostPath -> String
postPathToPath config (PostPath { filename, extension }) =
    String.join "/" [ config.workDir, "posts", filename ++ "." ++ extension ]


type ContentAddress
    = ContentAddress { filename : String, extension : String }


contentAddressToPath : ContentAddress -> String
contentAddressToPath (ContentAddress { filename, extension }) =
    filename ++ "." ++ extension


copyMediaToContentAddressableStorage : Config -> MediaPath -> BackendTask FatalError ContentAddress
copyMediaToContentAddressableStorage config ((MediaPath { extension }) as mediaPath) =
    copyToContentAddressableStorage config
        { path = mediaPathToPath config mediaPath
        , extension = extension
        }


copyPostToContentAddressableStorage : Config -> PostPath -> BackendTask FatalError ContentAddress
copyPostToContentAddressableStorage config ((PostPath { extension }) as postPath) =
    copyToContentAddressableStorage config
        { path = postPathToPath config postPath
        , extension = extension
        }


copyToContentAddressableStorage : Config -> { path : String, extension : String } -> BackendTask FatalError ContentAddress
copyToContentAddressableStorage config { path, extension } =
    Do.command "sha512sum" [ path ] <| \output ->
    let
        sum : String
        sum =
            output
                |> String.split " "
                |> List.take 1
                |> String.concat

        target : String
        target =
            config.outputDir ++ "/" ++ sum ++ "." ++ extension
    in
    Do.exec "cp" [ "-n", path, target ] <| \_ ->
    BackendTask.succeed (ContentAddress { filename = sum, extension = extension })


cache : Config -> String -> BackendTask FatalError ContentAddress
cache config urlString =
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

                scratchPath : ScratchPath
                scratchPath =
                    ScratchPath
                        { filename = hash
                        , extension = extension
                        }

                scratchTarget : String
                scratchTarget =
                    scratchPathToPath config scratchPath

                mediaPath : MediaPath
                mediaPath =
                    MediaPath
                        { filename = hash
                        , extension = extension
                        }

                mediaTarget : String
                mediaTarget =
                    mediaPathToPath config mediaPath
            in
            Do.glob mediaTarget <| \existing ->
            Do.do
                (if not (List.isEmpty existing) then
                    Do.noop

                 else
                    let
                        opts : List String
                        opts =
                            [ urlString, "-s", "-o", scratchTarget ]
                    in
                    Do.log (String.join " " ("curl" :: opts)) <| \_ ->
                    Do.exec "curl" opts <| \_ ->
                    Do.exec "mv" [ scratchTarget, mediaTarget ] <| \_ ->
                    Do.noop
                )
            <| \_ ->
            copyMediaToContentAddressableStorage config mediaPath


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


toNumber : Title -> Maybe String
toNumber title =
    case title of
        Demo number _ ->
            Maybe.map String.fromFloat number

        VoiceMemo _ ->
            Nothing

        BonusDemo _ ->
            Nothing

        SongIdea _ ->
            Nothing

        Podcast number _ ->
            Maybe.map String.fromInt number

        AnIdeaADay number _ ->
            case number of
                Ok n ->
                    Just (String.fromInt n)

                Err e ->
                    Just e

        FirstDraftFebruary number _ ->
            Maybe.map String.fromInt number

        AudioDiary n _ ->
            n

        Other _ ->
            Nothing
