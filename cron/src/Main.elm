module Main exposing (run)

import Ansi.Color
import Api
import BackendTask exposing (BackendTask)
import BackendTask.Custom as Custom
import BackendTask.Do as Do
import BackendTask.Env as Env
import BackendTask.Extra
import BackendTask.File as File
import BackendTask.Http as Http
import Cli.Option as Option
import Cli.OptionsParser as OptionsParser
import Cli.Program as Program
import Date exposing (Date)
import Dict exposing (Dict)
import Diff
import Diff.ToString
import FatalError exposing (FatalError)
import Json.Decode
import Json.Encode
import List.Extra
import Maybe.Extra
import Pages.Script as Script exposing (Script)
import Parser exposing ((|.), (|=), Parser, symbol)
import Parser.Error
import Parser.Workaround
import Result.Extra
import Rss exposing (Title(..))
import Rss.Parser
import Set
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
    Script.withCliOptions programConfig
        (\config ->
            BackendTask.Extra.setupDebugger
                |> BackendTask.and
                    (BackendTask.Extra.profile "main" (task config))
        )


programConfig : Program.Config Config
programConfig =
    Program.config
        |> Program.add
            (OptionsParser.build Config
                |> OptionsParser.with
                    (Option.requiredKeywordArg "output-dir"
                        |> Option.withDisplayName "dir"
                        |> Option.withDescription "The directory to output to."
                    )
                |> OptionsParser.with
                    (Option.optionalKeywordArg "work-dir"
                        |> Option.withDisplayName "dir"
                        |> Option.withDefault "work"
                        |> Option.withDescription "The directory to put intermediate files in.\nDefaults to `work`."
                    )
                |> OptionsParser.with
                    (Option.flag "force"
                        |> Option.withDescription "⚠️ Rewrite existing files ⚠️."
                    )
                |> OptionsParser.with
                    (Option.optionalKeywordArg "parallel"
                        |> Option.withDefault "10"
                        |> Option.withDisplayName "n"
                        |> Option.withDescription "How many files to download in parallel."
                        |> Option.validateMap
                            (\i ->
                                i
                                    |> String.toInt
                                    |> Result.fromMaybe "Invalid number "
                            )
                    )
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
        |> Spinner.Reader.withStep "Getting posts from the Patreon API (auth)"
            (\env _ ->
                Api.getPosts { workDir = config.workDir, cookie = Just env.cookie }
            )
        |> Spinner.Reader.withFatalStep "Downloading RSS feed"
            (\{ rssUrl } apiPosts ->
                BackendTask.map
                    (Tuple.pair apiPosts)
                    (Http.get rssUrl Http.expectString
                        |> BackendTask.quiet
                    )
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
        |> Spinner.Reader.withStep "Getting posts from the Patreon API (anon)"
            (\_ ( apiPosts, rssPosts ) ->
                Do.do (Api.getPosts { workDir = config.workDir, cookie = Nothing }) <| \anonPosts ->
                BackendTask.succeed ( apiPosts, rssPosts, anonPosts )
            )
        |> Spinner.Reader.withStep "Checking posts' list"
            (\_ (( apiPosts, rssPosts, _ ) as posts) ->
                let
                    apiDict : Dict String Api.Post
                    apiDict =
                        apiPosts
                            |> List.map
                                (\apiPost ->
                                    ( "https://www.patreon.com" ++ apiPost.attributes.patreonUrl, apiPost )
                                )
                            |> Dict.fromList

                    rssDict : Dict String Rss.Post
                    rssDict =
                        rssPosts
                            |> List.map
                                (\rssPost ->
                                    ( rssPost.link, rssPost )
                                )
                            |> Dict.fromList
                in
                case Dict.diff rssDict apiDict |> Dict.keys of
                    [] ->
                        BackendTask.succeed posts

                    missingInApi ->
                        ("Posts missing in API: " ++ String.join ", " missingInApi)
                            |> FatalError.fromString
                            |> BackendTask.fail
            )
        |> Spinner.Reader.withStep "Downloading media"
            (\_ ( apiPosts, _, anonPosts ) ->
                let
                    cachePosts :
                        List Api.Post
                        ->
                            BackendTask
                                FatalError
                                ( List { image : MediaPath, media : Maybe MediaPath, post : Api.Post }
                                , List String
                                )
                    cachePosts posts =
                        let
                            ( tasks, warnings ) =
                                posts
                                    |> List.filterMap
                                        (\post ->
                                            case cachePost config post of
                                                CanCache t ->
                                                    Just (Ok t)

                                                CannotCache e ->
                                                    Just (Err e)

                                                NotRelevant ->
                                                    Nothing
                                        )
                                    |> Result.Extra.partition
                        in
                        tasks
                            |> List.Extra.greedyGroupsOf config.parallel
                            |> List.map BackendTask.combine
                            |> BackendTask.sequence
                            |> BackendTask.map (\ps -> ( List.concat ps, warnings ))
                in
                Do.do (cachePosts apiPosts) <| \( cachedPosts, cachedErrors ) ->
                Do.do (cachePosts anonPosts) <| \( cachedAnonPosts, cachedAnonErrors ) ->
                BackendTask.succeed
                    ( cachedPosts
                    , cachedAnonPosts
                    , (cachedErrors ++ cachedAnonErrors)
                        |> Set.fromList
                        |> Set.toList
                    )
            )
        |> Spinner.Reader.withStep "Formatting posts"
            (\_ ( cachedPosts, cachedAnonPosts, warnings ) ->
                let
                    formatPosts :
                        { anonymous : Bool }
                        ->
                            List
                                { image : MediaPath
                                , media : Maybe MediaPath
                                , post : Api.Post
                                }
                        -> BackendTask FatalError (List ParsedPost)
                    formatPosts anonymous posts =
                        posts
                            |> List.map
                                (\post ->
                                    postToContent config anonymous post
                                )
                            |> BackendTask.combine
                in
                Do.do (formatPosts { anonymous = False } cachedPosts) <| \formatted ->
                Do.do (formatPosts { anonymous = True } cachedAnonPosts) <| \formattedAnon ->
                BackendTask.succeed ( formatted, formattedAnon, warnings )
            )
        |> Spinner.Reader.withStep "Checking anon posts"
            (\_ ( formatted, formattedAnon, warnings ) ->
                let
                    loggedInDict : Dict String ParsedPost
                    loggedInDict =
                        formatted
                            |> List.map
                                (\formattedPost ->
                                    ( formattedPost.link, formattedPost )
                                )
                            |> Dict.fromList

                    anonDict : Dict String ParsedPost
                    anonDict =
                        formattedAnon
                            |> List.map
                                (\formattedPost ->
                                    ( formattedPost.link, formattedPost )
                                )
                            |> Dict.fromList

                    anonCheck : Result String (List String)
                    anonCheck =
                        Dict.merge
                            (\_ _ acc -> acc)
                            (\k l r a ->
                                let
                                    clean : ParsedPost -> ParsedPost
                                    clean p =
                                        { p | media = Nothing, anonymous = False, content = Nothing }
                                in
                                if clean l /= clean r then
                                    Err
                                        ("Post is different in anon/auth feed: "
                                            ++ k
                                            ++ "\n"
                                            ++ toStringDiff Debug.toString (clean l) (clean r)
                                        )

                                else if r.media /= Nothing then
                                    Result.map ((::) ("Media in anonymous post: " ++ k)) a

                                else
                                    a
                            )
                            (\k _ _ -> Err ("Post missing in authenticated feed: " ++ k))
                            loggedInDict
                            anonDict
                            (Ok [])
                in
                case anonCheck of
                    Ok newWarnings ->
                        BackendTask.succeed ( formatted, formattedAnon, warnings ++ newWarnings )

                    Err e ->
                        BackendTask.fail (FatalError.fromString e)
            )
        |> Spinner.Reader.withStep "Writing dump"
            (\_ ( formatted, _, warnings ) ->
                let
                    body : String
                    body =
                        formatted
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
                BackendTask.succeed warnings
            )
        |> Spinner.Reader.withStep "Re-reading the posts from disk to apply overrides"
            (\_ warnings ->
                Do.glob (config.workDir ++ "/posts/*/*/*.md") <| \filenames ->
                Do.each filenames parsePost <| \parsed ->
                BackendTask.succeed ( parsed, warnings )
            )
        |> Spinner.Reader.withStep "Copying media to content storage"
            (\_ ( parsedPosts, warnings ) ->
                parsedPosts
                    |> List.map
                        (\post ->
                            Do.do
                                (case post.media of
                                    Nothing ->
                                        BackendTask.succeed Nothing

                                    Just media ->
                                        copyMediaToContentAddressableStorage config media
                                            |> BackendTask.map Just
                                )
                            <| \media ->
                            Do.do (copyMediaToContentAddressableStorage config post.image) <| \image ->
                            { post = post
                            , media = media
                            , image = image
                            }
                                |> BackendTask.succeed
                        )
                    |> BackendTask.combine
                    |> BackendTask.map
                        (\posts ->
                            ( List.sortBy (\{ post } -> Time.posixToMillis post.date) posts
                            , warnings
                            )
                        )
            )
        |> Spinner.Reader.withStep "Writing indexes"
            (\_ ( finalPosts, warnings ) ->
                let
                    writeTierIndex : Maybe Tier -> BackendTask FatalError ( String, ContentAddress )
                    writeTierIndex maybeTier =
                        let
                            tierName : String
                            tierName =
                                case maybeTier of
                                    Just tier ->
                                        tierToString tier

                                    Nothing ->
                                        "Anonymous"

                            path : String
                            path =
                                config.workDir ++ "/index-" ++ String.toLower tierName ++ ".md"
                        in
                        Do.each
                            (finalPosts
                                |> List.filter
                                    (\{ post } ->
                                        case maybeTier of
                                            Just tier ->
                                                not post.anonymous && List.member tier post.tiers

                                            Nothing ->
                                                post.anonymous
                                    )
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
                        Do.do
                            (copyToContentAddressableStorage config
                                { prefix = String.toLower tierName
                                , path = path
                                , extension = "md"
                                }
                            )
                        <|
                            \address -> BackendTask.succeed ( tierName, address )
                in
                Do.do
                    ([ Nothing, Just Bronze, Just Silver, Just Gold ]
                        |> List.map writeTierIndex
                        |> BackendTask.combine
                    )
                <| \tiers ->
                BackendTask.succeed ( tiers, warnings )
            )
        |> Spinner.Reader.withStep "Cleaning up"
            (\_ ( indexAddresses, warnings ) ->
                Do.command "rm" [ "-r", "work/media-scratch" ] <| \_ ->
                BackendTask.succeed ( indexAddresses, warnings )
            )
        |> Spinner.Reader.runSteps
        |> BackendTask.andThen
            (\( tiers, warnings ) ->
                let
                    tiersOutput : List String
                    tiersOutput =
                        List.map
                            (\( name, address ) ->
                                " - " ++ name ++ ": " ++ contentAddressToPath address
                            )
                            tiers
                in
                Script.log (String.join "\n" (tiersOutput ++ warnings))
            )


toStringDiff : (a -> String) -> a -> a -> String
toStringDiff toString l r =
    let
        ls : String
        ls =
            toString l
                |> String.replace "," ",\n"

        rs : String
        rs =
            toString r
                |> String.replace "," ",\n"
    in
    Diff.diffLines ls rs
        |> List.map expandChange
        |> Diff.ToString.diffToString
            { context = 3, color = True }


expandChange :
    Diff.Change () String
    -> Diff.Change (List (Diff.Change Never Char)) String
expandChange change =
    case change of
        Diff.Added a ->
            Diff.Added a

        Diff.Removed r ->
            Diff.Removed r

        Diff.Similar l r _ ->
            Diff.Similar l r (Diff.diff (String.toList l) (String.toList r))

        Diff.NoChange n ->
            Diff.NoChange n


type alias ParsedPost =
    { title : String
    , category : String
    , date : Time.Posix
    , image : MediaPath
    , link : String
    , media : Maybe MediaPath
    , tiers : List Tier
    , number : Maybe String
    , content : Maybe String
    , anonymous : Bool
    }


parsePost : String -> BackendTask FatalError ParsedPost
parsePost path =
    Do.allowFatal (File.rawFile path) <| \raw ->
    Parser.run (postFileParser { anonymous = String.contains "anonymous-" path }) raw
        |> Result.mapError
            (\e ->
                Parser.Error.renderError
                    { text = identity
                    , formatContext = Ansi.Color.fontColor Ansi.Color.cyan
                    , formatCaret = Ansi.Color.fontColor Ansi.Color.red
                    , newline = "\n"
                    , linesOfExtraContext = 3
                    }
                    Parser.Error.forParser
                    raw
                    e
                    |> String.concat
                    |> FatalError.fromString
            )
        |> BackendTask.fromResult


postFileParser : { anonymous : Bool } -> Parser ParsedPost
postFileParser { anonymous } =
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
            , anonymous = anonymous
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
        |= Parser.oneOf
            [ Parser.map (\t m -> Just (t m)) (mediaRow "Media")
            , Parser.succeed (\_ -> Nothing)
            ]
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
        |> Parser.andThen
            (\p ->
                if p.media == Nothing && not p.anonymous then
                    Parser.problem (missingFileError p)

                else
                    Parser.succeed p
            )


postToDumpFragment : ParsedPost -> String
postToDumpFragment post =
    "<a href=\""
        ++ post.link
        ++ "\"><h2>"
        ++ post.title
        ++ "</h2></a><p>"
        ++ Maybe.withDefault "" post.content
        ++ "</p>"


type CacheResult
    = CannotCache String
    | CanCache (BackendTask FatalError { image : MediaPath, media : Maybe MediaPath, post : Api.Post })
    | NotRelevant


cachePost : Config -> Api.Post -> CacheResult
cachePost config post =
    case post.attributes.postType of
        Api.Podcast ->
            let
                isOneTimePayment : Bool
                isOneTimePayment =
                    List.member Api.UnlockByPostOneTimePayment post.relationships.contentUnlockOptions
            in
            if isOneTimePayment then
                NotRelevant

            else
                let
                    mediaUrlResult : Result String (Maybe Url)
                    mediaUrlResult =
                        case post.attributes.postFile of
                            Just (Api.PostFileAudioVideo { url }) ->
                                case url of
                                    Nothing ->
                                        Ok Nothing

                                    Just u ->
                                        Ok (Just u)

                            Nothing ->
                                Ok Nothing

                            Just (Api.PostFileImage _) ->
                                Err ("Post " ++ post.id ++ ", expecting an audio/video file, found image")

                            Just (Api.PostFileSize _) ->
                                Err ("Post " ++ post.id ++ ", expecting an audio/video file, found size")

                    thumbSquareUrlResult : Result String Url
                    thumbSquareUrlResult =
                        case post.attributes.thumbnail of
                            Just (Api.Thumbnail_Square square) ->
                                Ok square.thumbnail

                            Just (Api.Thumbnail_Gif _) ->
                                Err ("Post " ++ post.id ++ " has a GIF thumbnail")

                            Nothing ->
                                Err ("Post " ++ post.id ++ " does not have a thumbnail")
                in
                case ( mediaUrlResult, thumbSquareUrlResult ) of
                    ( Err e, _ ) ->
                        CannotCache e

                    ( _, Err e ) ->
                        CannotCache e

                    ( Ok mediaUrl, Ok thumbUrl ) ->
                        (Do.do (cache config post thumbUrl) <| \image ->
                        Do.do
                            (case mediaUrl of
                                Nothing ->
                                    BackendTask.succeed Nothing

                                Just url ->
                                    BackendTask.map Just (cache config post url)
                            )
                        <| \media ->
                        BackendTask.succeed { image = image, media = media, post = post }
                        )
                            |> CanCache

        Api.LivestreamYoutube ->
            NotRelevant

        Api.TextOnly ->
            NotRelevant

        Api.ImageFile ->
            NotRelevant

        Api.Link ->
            NotRelevant

        Api.VideoEmbed ->
            NotRelevant

        Api.VideoExternalFile ->
            NotRelevant

        Api.Poll ->
            NotRelevant

        Api.LivestreamCrowdcast ->
            NotRelevant

        Api.AudioEmbed ->
            NotRelevant


missingFileError : ParsedPost -> String
missingFileError post =
    "Post \"" ++ post.title ++ "\" - " ++ post.link ++ ", missing file\n" ++ Debug.toString post


taskFromResult : Result String a -> BackendTask FatalError a
taskFromResult result =
    result
        |> Result.mapError FatalError.fromString
        |> BackendTask.fromResult


postToContent :
    Config
    -> { anonymous : Bool }
    -> { image : MediaPath, media : Maybe MediaPath, post : Api.Post }
    -> BackendTask FatalError ParsedPost
postToContent config anonymous { image, media, post } =
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
            postPathToPath config anonymous postPath

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
                , anonymous = anonymous.anonymous
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
    , Maybe.map (\media -> "Media: " ++ mediaPathToFilename media) post.media
    , Just ("Tiers: " ++ String.join ", " (List.map tierToString post.tiers))
    , Maybe.map (\num -> "Number: " ++ num) post.number
    , Maybe.map (\content -> "Content: " ++ content) post.content
    ]
        |> List.filterMap identity
        |> String.join "\n"


parsedPostToFinalString : { post : ParsedPost, image : ContentAddress, media : Maybe ContentAddress } -> String
parsedPostToFinalString { post, image, media } =
    [ Just ("Title: " ++ post.title)
    , Just ("Category: " ++ post.category)
    , Just ("Date: " ++ String.fromInt (Time.posixToMillis post.date))
    , Just ("Image: " ++ contentAddressToPath image)
    , Just ("Link: " ++ post.link)
    , Maybe.map (\m -> "Media: " ++ contentAddressToPath m) media
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


postPathToPath : Config -> { anonymous : Bool } -> PostPath -> String
postPathToPath config { anonymous } ((PostPath inner) as path) =
    String.join "/"
        [ postPathToDir config path
        , if anonymous then
            "anonymous-" ++ pathToFilename inner

          else
            pathToFilename inner
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
        , prefix = "media"
        }


copyToContentAddressableStorage : Config -> { prefix : String, path : String, extension : String } -> BackendTask FatalError ContentAddress
copyToContentAddressableStorage config { prefix, path, extension } =
    Do.command "sha512sum" [ path ] <| \output ->
    let
        sum : String
        sum =
            output
                |> String.split " "
                |> List.take 1
                |> String.concat

        filename : String
        filename =
            prefix ++ "-" ++ sum

        target : String
        target =
            config.outputDir ++ "/" ++ filename ++ "." ++ extension
    in
    Do.exec "cp" [ "-n", path, target ] <| \_ ->
    BackendTask.succeed (ContentAddress { filename = filename, extension = extension })


cache : Config -> Api.Post -> Url -> BackendTask FatalError MediaPath
cache config post url =
    let
        urlString : String
        urlString =
            Url.toString url

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

        WritingSomethingEveryDay number _ ->
            Just (String.fromInt number)

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
