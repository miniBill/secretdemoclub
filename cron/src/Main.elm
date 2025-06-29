module Main exposing (run)

import Api
import BackendTask exposing (BackendTask)
import BackendTask.Custom as Custom
import BackendTask.Do as Do
import BackendTask.Env as Env
import BackendTask.File as File
import BackendTask.Http as Http
import Cli.Option as Option
import Cli.OptionsParser as OptionsParser
import Cli.Program as Program
import Date exposing (Date)
import FatalError exposing (FatalError)
import Json.Decode
import Json.Encode
import List.Extra
import Maybe.Extra
import Pages.Script as Script exposing (Script)
import Parser exposing ((|.), (|=), Parser, symbol)
import Parser.Extra
import Parser.Workaround
import Result.Extra
import Rss exposing (Title(..))
import Rss.Parser
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
                Do.exec "rm" [ "-rf", config.workDir ++ "/media-scratch" ] <| \_ ->
                Do.exec "mkdir" [ "-p", config.workDir ++ "/media-scratch" ] <| \_ ->
                Do.exec "mkdir" [ "-p", config.workDir ++ "/posts" ] <| \_ ->
                Do.exec "mkdir" [ "-p", config.outputDir ] <| \_ ->
                Do.noop
            )
        |> Spinner.Reader.withStep "Applying overrides"
            (\_ _ ->
                Script.command "cp" [ "-r", "overrides/posts", config.workDir ]
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
        |> Spinner.Reader.withStep "Formatting posts"
            (\_ cachedPosts ->
                cachedPosts
                    |> List.map (postToContent config)
                    |> BackendTask.combine
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
        |> Spinner.Reader.withStep "Re-reading the posts from disk to apply overrides"
            (\_ _ ->
                Do.glob (config.workDir ++ "/posts/*/*/*.md") <| \filenames ->
                Do.each filenames parsePost <| \parsed ->
                BackendTask.succeed parsed
            )
        |> Spinner.Reader.withStep "Copying media to content storage"
            (\_ parsedPosts ->
                parsedPosts
                    |> List.map
                        (\post ->
                            Do.do (copyMediaToContentAddressableStorage config post.media) <| \media ->
                            Do.do (copyMediaToContentAddressableStorage config post.image) <| \image ->
                            { post = post
                            , media = media
                            , image = image
                            }
                                |> BackendTask.succeed
                        )
                    |> BackendTask.combine
                    |> BackendTask.map (\posts -> List.sortBy (\{ post } -> Time.posixToMillis post.date) posts)
            )
        |> Spinner.Reader.withStep "Writing indexes"
            (\_ finalPosts ->
                [ Bronze, Silver, Gold ]
                    |> List.map
                        (\tier ->
                            let
                                path : String
                                path =
                                    config.workDir ++ "/index-" ++ String.toLower (tierToString tier) ++ ".md"
                            in
                            Do.each
                                (finalPosts
                                    |> List.filter (\{ post } -> List.member tier post.tiers)
                                )
                                (\post ->
                                    parsedPostToFinalString post
                                        |> BackendTask.succeed
                                )
                            <| \formattedPosts ->
                            Do.allowFatal
                                (Script.writeFile
                                    { path = path
                                    , body = String.join "\n\n" formattedPosts
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
                Do.command "rm" [ "-r", "work/media-scratch" ] <| \_ ->
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


type alias ParsedPost =
    { title : String
    , category : String
    , date : Time.Posix
    , image : MediaPath
    , link : String
    , media : MediaPath
    , tiers : List Tier
    , number : Maybe String
    , content : Maybe String
    }


parsePost : String -> BackendTask FatalError ParsedPost
parsePost path =
    Do.allowFatal (File.rawFile path) <| \raw ->
    Parser.run postFileParser raw
        |> Result.mapError (\e -> FatalError.fromString (Parser.Extra.errorsToString raw e))
        |> BackendTask.fromResult


postFileParser : Parser ParsedPost
postFileParser =
    let
        row : String -> (String -> Result String a) -> Parser a
        row key postChomp =
            Parser.succeed identity
                |. Parser.keyword key
                |. Parser.symbol ":"
                |= (Parser.Workaround.chompUntilEndOrBefore "\n"
                        |> Parser.getChompedString
                        |> Parser.andThen
                            (\chomped ->
                                case postChomp (String.trim chomped) of
                                    Ok r ->
                                        Parser.succeed r

                                    Err e ->
                                        Parser.problem e
                            )
                   )
                |. Parser.spaces

        mediaRow : String -> Parser (Time.Posix -> MediaPath)
        mediaRow key =
            row key
                (\raw ->
                    case
                        raw
                            |> String.split "."
                            |> List.reverse
                    of
                        extension :: rest ->
                            Ok
                                (\date ->
                                    MediaPath
                                        { date = Date.fromPosix Time.utc date
                                        , extension = extension
                                        , filename = String.join "." (List.reverse rest)
                                        }
                                )

                        [] ->
                            Err "Invalid empty path"
                )
    in
    Parser.succeed
        (\title category date image link media tiers number content ->
            { title = title
            , category = category
            , date = date
            , image = image date
            , link = link
            , media = media date
            , tiers = tiers
            , number = number
            , content = content
            }
        )
        |= row "Title" Ok
        |= row "Category" Ok
        |= row "Date"
            (\raw ->
                case String.toInt raw of
                    Just millis ->
                        Ok (Time.millisToPosix millis)

                    Nothing ->
                        Err ("Invalid int: " ++ raw)
            )
        |= mediaRow "Image"
        |= row "Link" Ok
        |= mediaRow "Media"
        |= row "Tiers"
            (\raw ->
                raw
                    |> String.split ","
                    |> List.map String.trim
                    |> Maybe.Extra.combineMap tierFromString
                    |> Result.fromMaybe ("Invalid tiers: " ++ raw)
            )
        |= Parser.oneOf
            [ Parser.map Just (row "Number" Ok)
            , Parser.succeed Nothing
            ]
        |= Parser.oneOf
            [ Parser.succeed (\raw -> Just (String.trim raw))
                |. symbol "Content: "
                |= Parser.getChompedString (Parser.chompWhile (\_ -> True))
            , Parser.succeed Nothing
            ]


postToDumpFragment : ParsedPost -> String
postToDumpFragment post =
    "<a href=\""
        ++ post.link
        ++ "\"><h2>"
        ++ post.title
        ++ "</h2></a><p>"
        ++ Maybe.withDefault "" post.content
        ++ "</p>"


cachePost : Config -> Api.Post -> BackendTask FatalError (Maybe { image : MediaPath, media : MediaPath, post : Api.Post })
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
                        Api.Thumbnail_Square square ->
                            Ok square.thumbnail

                        Api.Thumbnail_Gif _ ->
                            Err ("Post " ++ post.id ++ " has a GIF thumbnail")
            in
            Do.do (taskFromResult mediaUrlResult) <| \mediaUrl ->
            Do.do (taskFromResult thumbSquareUrlResult) <| \thumbUrl ->
            Do.do (cache config post (Url.toString thumbUrl)) <| \image ->
            Do.do (cache config post (Url.toString mediaUrl)) <| \media ->
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


postToContent : Config -> { image : MediaPath, media : MediaPath, post : Api.Post } -> BackendTask FatalError ParsedPost
postToContent config { image, media, post } =
    let
        postPath : PostPath
        postPath =
            PostPath
                { date = Date.fromPosix Time.utc post.attributes.publishedAt
                , filename =
                    post.attributes.title
                        |> Maybe.withDefault "unnamed"
                , extension = "md"
                }

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
    in
    Do.do (taskFromResult (getTier post)) <| \tiers ->
    Do.allowFatal (fileExists target) <| \exists ->
    if exists && not config.force then
        parsePost target

    else
        let
            parsedPost : ParsedPost
            parsedPost =
                { title = Rss.titleToString title
                , category = Rss.getAlbum title
                , date = post.attributes.publishedAt
                , image = image
                , link = "https://www.patreon.com" ++ post.attributes.patreonUrl
                , media = media
                , tiers = tiers
                , number = toNumber title
                , content = post.attributes.content
                }

            body : String
            body =
                parsedPostToString parsedPost
        in
        Script.writeFile
            { path = target
            , body = body
            }
            |> BackendTask.allowFatal
            |> BackendTask.map (\_ -> parsedPost)


parsedPostToString : ParsedPost -> String
parsedPostToString post =
    [ Just ("Title: " ++ post.title)
    , Just ("Category: " ++ post.category)
    , Just ("Date: " ++ String.fromInt (Time.posixToMillis post.date))
    , Just ("Image: " ++ mediaPathToFilename post.image)
    , Just ("Link: " ++ post.link)
    , Just ("Media: " ++ mediaPathToFilename post.media)
    , Just ("Tiers: " ++ String.join ", " (List.map tierToString post.tiers))
    , Maybe.map (\num -> "Number: " ++ num) post.number
    , Maybe.map (\content -> "Content: " ++ content) post.content
    ]
        |> List.filterMap identity
        |> String.join "\n"


parsedPostToFinalString : { post : ParsedPost, image : ContentAddress, media : ContentAddress } -> String
parsedPostToFinalString { post, image, media } =
    [ Just ("Title: " ++ post.title)
    , Just ("Category: " ++ post.category)
    , Just ("Date: " ++ String.fromInt (Time.posixToMillis post.date))
    , Just ("Image: " ++ contentAddressToPath image)
    , Just ("Link: " ++ post.link)
    , Just ("Media: " ++ contentAddressToPath media)
    , Maybe.map (\num -> "Number: " ++ num) post.number
    ]
        |> List.filterMap identity
        |> String.join "\n"


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
    = ScratchPath { date : Date, filename : String, extension : String }


scratchPathToPath : Config -> ScratchPath -> String
scratchPathToPath config ((ScratchPath inner) as path) =
    String.join "/"
        [ scratchPathToDir config path
        , pathToFilename inner
        ]


pathToFilename : { a | filename : String, extension : String } -> String
pathToFilename { filename, extension } =
    (filename ++ "." ++ extension)
        |> String.replace "/" "_"


scratchPathToDir : Config -> ScratchPath -> String
scratchPathToDir config (ScratchPath { date }) =
    String.join "/"
        [ config.workDir
        , "media-scratch"
        , String.fromInt (Date.year date)
        , Date.toIsoString date
        ]


type MediaPath
    = MediaPath { date : Date, filename : String, extension : String }


mediaPathToPath : Config -> MediaPath -> String
mediaPathToPath config path =
    String.join "/"
        [ mediaPathToDir config path
        , mediaPathToFilename path
        ]


mediaPathToDir : Config -> MediaPath -> String
mediaPathToDir config (MediaPath { date }) =
    String.join "/"
        [ config.workDir
        , "posts"
        , String.fromInt (Date.year date)
        , Date.toIsoString date
        ]


mediaPathToFilename : MediaPath -> String
mediaPathToFilename (MediaPath inner) =
    pathToFilename inner


type PostPath
    = PostPath { date : Date, filename : String, extension : String }


postPathToPath : Config -> PostPath -> String
postPathToPath config ((PostPath inner) as path) =
    String.join "/"
        [ postPathToDir config path
        , pathToFilename inner
        ]


postPathToDir : Config -> PostPath -> String
postPathToDir config (PostPath { date }) =
    String.join "/"
        [ config.workDir
        , "posts"
        , String.fromInt (Date.year date)
        , Date.toIsoString date
        ]


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


cache : Config -> Api.Post -> String -> BackendTask FatalError MediaPath
cache config post urlString =
    case Url.fromString urlString of
        Nothing ->
            BackendTask.fail (FatalError.fromString ("Invalid URL: " ++ urlString))

        Just url ->
            let
                extension : String
                extension =
                    url.path
                        |> String.split "."
                        |> List.reverse
                        |> List.head
                        |> Maybe.withDefault "???"

                date : Date
                date =
                    Date.fromPosix Time.utc post.attributes.publishedAt

                filename : String
                filename =
                    post.attributes.title
                        |> Maybe.withDefault "unnamed"

                scratchPath : ScratchPath
                scratchPath =
                    ScratchPath
                        { date = date
                        , filename = filename
                        , extension = extension
                        }

                scratchTarget : String
                scratchTarget =
                    scratchPathToPath config scratchPath

                scratchDir : String
                scratchDir =
                    scratchPathToDir config scratchPath

                mediaPath : MediaPath
                mediaPath =
                    MediaPath
                        { date = date
                        , filename = filename
                        , extension = extension
                        }

                mediaTarget : String
                mediaTarget =
                    mediaPathToPath config mediaPath

                mediaDir : String
                mediaDir =
                    mediaPathToDir config mediaPath
            in
            Do.allowFatal (fileExists mediaTarget) <| \exists ->
            Do.do
                (if exists then
                    Do.noop

                 else
                    let
                        opts : List String
                        opts =
                            [ urlString, "-s", "-o", scratchTarget ]
                    in
                    Do.log (String.join " " ("curl" :: opts)) <| \_ ->
                    Do.exec "mkdir" [ "-p", scratchDir ] <| \_ ->
                    Do.exec "mkdir" [ "-p", mediaDir ] <| \_ ->
                    Do.exec "curl" opts <| \_ ->
                    Do.exec "mv" [ scratchTarget, mediaTarget ] <| \_ ->
                    Do.noop
                )
            <| \_ ->
            BackendTask.succeed mediaPath


fileExists :
    String
    -> BackendTask { fatal : FatalError, recoverable : Custom.Error } Bool
fileExists path =
    Custom.run "fileExists" (Json.Encode.string path) Json.Decode.bool
        |> BackendTask.quiet


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


tierFromString : String -> Maybe Tier
tierFromString tier =
    case tier of
        "Bronze" ->
            Just Bronze

        "Silver" ->
            Just Silver

        "Gold" ->
            Just Gold

        _ ->
            Nothing
