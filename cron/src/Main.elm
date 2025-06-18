module Main exposing (run)

import Api
import BackendTask exposing (BackendTask)
import BackendTask.Do as Do
import BackendTask.Env as Env
import BackendTask.File as File
import BackendTask.Http as Http
import Cli.Option as Option
import Cli.OptionsParser as OptionsParser
import Cli.Program as Program
import FatalError exposing (FatalError)
import List.Extra
import Pages.Script as Script exposing (Script)
import Parser
import Result.Extra
import Rss exposing (Title(..))
import Rss.Parser
import SHA256
import Set exposing (Set)
import Spinner.Reader
import String.Multiline
import Time
import Url exposing (Url)


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
        |> Spinner.Reader.withFatalStep "Downloading RSS feed"
            (\{ rssUrl } apiPosts ->
                BackendTask.map
                    (Tuple.pair apiPosts)
                    (Http.get rssUrl Http.expectString)
            )
        |> Spinner.Reader.withStep "Parsing RSS feed"
            (\_ ( apiPosts, xml ) ->
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

                    Ok rssPosts ->
                        BackendTask.succeed ( apiPosts, rssPosts )
            )
        |> Spinner.Reader.withStep "Checking posts' list"
            (\_ ( apiPosts, rssPosts ) ->
                let
                    apiSet : Set String
                    apiSet =
                        apiPosts
                            |> List.map
                                (\apiPost ->
                                    "https://www.patreon.com" ++ apiPost.attributes.patreonUrl
                                )
                            |> Set.fromList

                    rssSet : Set String
                    rssSet =
                        rssPosts
                            |> List.map
                                (\rssPost ->
                                    rssPost.link
                                )
                            |> Set.fromList
                in
                -- case Set.diff apiSet rssSet |> Set.toList of
                --     [] ->
                --         BackendTask.succeed ( apiPosts, rssPosts )
                --     missingInRss ->
                --         BackendTask.fail
                --             (FatalError.fromString
                --                 ("Posts missing in RSS: " ++ String.join ", " missingInRss)
                --             )
                case Set.diff rssSet apiSet |> Set.toList of
                    [] ->
                        BackendTask.succeed ( apiPosts, rssPosts )

                    missingInApi ->
                        BackendTask.fail
                            (FatalError.fromString
                                ("Posts missing in API: " ++ String.join ", " missingInApi)
                            )
            )
        |> Spinner.Reader.withStep "Downloading media"
            (\_ ( apiPosts, _ ) ->
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
        |> Spinner.Reader.withStep "Writing dump"
            (\_ cachedPosts ->
                let
                    body : String
                    body =
                        cachedPosts
                            |> List.map postToDumpFragment
                            |> String.join "\n"

                    header : String
                    header =
                        String.Multiline.here
                            """
                            <!DOCTYPE html>
                            <html lang="en">
                            <head>
                                <meta charset="UTF-8">
                                <meta name="viewport" content="width=device-width, initial-scale=1.0">
                                <title>Patreon Dump</title>
                            </head>
                            <body>
                            """

                    footer : String
                    footer =
                        String.Multiline.here
                            """
                            </body>
                            </html>
                            """
                in
                Do.allowFatal
                    (Script.writeFile
                        { path = "work/dump.html"
                        , body = header ++ body ++ footer
                        }
                    )
                <| \_ ->
                BackendTask.succeed cachedPosts
            )
        |> Spinner.Reader.withStep "Formatting posts"
            (\_ cachedPosts ->
                cachedPosts
                    |> List.map (postToContent config)
                    |> BackendTask.combine
            )
        |> Spinner.Reader.withStep "Writing indexes"
            (\_ formattedPosts ->
                [ Bronze, Silver, Gold ]
                    |> List.map
                        (\tier ->
                            let
                                path : String
                                path =
                                    config.workDir ++ "/index-" ++ String.toLower (tierToString tier) ++ ".md"

                                body : String
                                body =
                                    formattedPosts
                                        |> List.filterMap
                                            (\( content, postTiers ) ->
                                                if List.member tier postTiers then
                                                    Just content

                                                else
                                                    Nothing
                                            )
                                        |> String.join "\n\n"
                            in
                            Do.allowFatal
                                (Script.writeFile
                                    { path = path
                                    , body = body
                                    }
                                )
                            <| \_ ->
                            Do.do (copyToContentAddressableStorage config { path = path, extension = "md" }) <| \indexAddress ->
                            BackendTask.succeed ( tier, indexAddress )
                        )
                    |> BackendTask.combine
            )
        |> Spinner.Reader.withStep "Cleaning up"
            (\_ indexAddresses ->
                Do.command "rmdir" [ "work/media-scratch" ] <| \_ ->
                BackendTask.succeed indexAddresses
            )
        |> Spinner.Reader.runSteps
        |> BackendTask.andThen
            (\indexAddresses ->
                indexAddresses
                    |> List.map
                        (\( tier, indexAddress ) ->
                            Script.log
                                (String.toLower (tierToString tier)
                                    ++ "_tier = \""
                                    ++ contentAddressToPath indexAddress
                                    ++ "\""
                                )
                        )
                    |> BackendTask.doEach
            )


postToDumpFragment : { a | post : Api.Post } -> String
postToDumpFragment { post } =
    "<a href=\"https://www.patreon.com"
        ++ post.attributes.patreonUrl
        ++ "\"><h2>"
        ++ Maybe.withDefault "???" post.attributes.title
        ++ "</h2></a><p>"
        ++ Maybe.withDefault "" post.attributes.content
        ++ "</p>"


cachePost : Config -> Api.Post -> BackendTask FatalError (Maybe { image : ContentAddress, media : ContentAddress, post : Api.Post })
cachePost config post =
    case post.attributes.postType of
        Api.Podcast ->
            let
                mediaUrlResult : Result String Url
                mediaUrlResult =
                    case post.attributes.postFile of
                        Just (Api.PostFileAudioVideo { url }) ->
                            Ok url

                        Nothing ->
                            Err ("Post " ++ post.id ++ ", missing file")

                        Just (Api.PostFileImage _) ->
                            Err ("Post " ++ post.id ++ ", expecting an audio/video file, found image")

                        Just (Api.PostFileSize _) ->
                            Err ("Post " ++ post.id ++ ", expecting an audio/video file, found size")

                thumbSquareUrlResult : Result String Url
                thumbSquareUrlResult =
                    case post.attributes.thumbnail of
                        -- Nothing ->
                        --     Err ("Post " ++ post.id ++ ", missing attributes.thumbnail")
                        -- Just
                        thumbnail ->
                            case thumbnail of
                                Api.Thumbnail_Square square ->
                                    Ok square.thumbnail

                                Api.Thumbnail_Gif _ ->
                                    Err ("Post " ++ post.id ++ " has a GIF thumbnail")
            in
            Do.do (taskFromResult mediaUrlResult) <| \mediaUrl ->
            Do.do (taskFromResult thumbSquareUrlResult) <| \thumbUrl ->
            Do.do (cache config (Url.toString thumbUrl)) <| \image ->
            Do.do (cache config (Url.toString mediaUrl)) <| \media ->
            BackendTask.succeed (Just { image = image, media = media, post = post })

        Api.LivestreamYoutube ->
            BackendTask.succeed Nothing

        Api.TextOnly ->
            BackendTask.succeed Nothing

        Api.ImageFile ->
            BackendTask.succeed Nothing

        Api.Link ->
            BackendTask.succeed Nothing

        Api.VideoEmbed ->
            BackendTask.succeed Nothing

        Api.VideoExternalFile ->
            BackendTask.succeed Nothing

        Api.Poll ->
            BackendTask.succeed Nothing

        Api.LivestreamCrowdcast ->
            BackendTask.succeed Nothing

        Api.AudioEmbed ->
            BackendTask.succeed Nothing


taskFromResult : Result String a -> BackendTask FatalError a
taskFromResult result =
    result
        |> Result.mapError FatalError.fromString
        |> BackendTask.fromResult


postToContent : Config -> { image : ContentAddress, media : ContentAddress, post : Api.Post } -> BackendTask FatalError ( String, List Tier )
postToContent config { image, media, post } =
    getTier post
        |> taskFromResult
        |> BackendTask.andThen
            (\tiers ->
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
                        , Just ("Category: " ++ Rss.getAlbum title)
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
                Do.do
                    (if not config.force && not (List.isEmpty existing) then
                        File.rawFile target
                            |> BackendTask.allowFatal

                     else
                        Script.writeFile
                            { path = target
                            , body = body
                            }
                            |> BackendTask.allowFatal
                            |> BackendTask.map (\_ -> body)
                    )
                <| \onDisk ->
                BackendTask.succeed ( onDisk, tiers )
            )


getTier : Api.Post -> Result String (List Tier)
getTier post =
    case
        post.relationships.accessRules
            |> List.filterMap .reward
            |> Result.Extra.combineMap
                (\reward ->
                    case String.trim reward.title of
                        "Gold membership" ->
                            Ok Gold

                        "Silver membership" ->
                            Ok Silver

                        "Bronze membership" ->
                            Ok Bronze

                        title ->
                            Err title
                )
            |> Result.map (List.Extra.unique >> List.sortBy tierToString)
    of
        Err e ->
            Err e

        Ok [] ->
            Ok [ Bronze, Gold, Silver ]

        Ok [ Gold ] ->
            Ok [ Gold ]

        Ok [ Gold, Silver ] ->
            Ok [ Gold, Silver ]

        Ok [ Bronze, Gold, Silver ] ->
            Ok [ Bronze, Gold, Silver ]

        Ok tiers ->
            Err ("Unexpected tiers: " ++ String.join ", " (List.map tierToString tiers))


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


type Tier
    = Bronze
    | Silver
    | Gold


tierToString : Tier -> String
tierToString tier =
    case tier of
        Bronze ->
            "Bronze"

        Silver ->
            "Silver"

        Gold ->
            "Gold"
