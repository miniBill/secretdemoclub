module Rss.Parser exposing (Error(..), ErrorWithPath, Path, errorToString, parse, rssPrefix)

import Imf.DateTime
import List.Extra
import Parser exposing ((|.), (|=), Parser)
import Result.Extra
import Rss exposing (Post, Title(..))
import Time
import XmlParser exposing (Attribute, Node(..))


parse : String -> Result ErrorWithPath (List Post)
parse xml =
    case XmlParser.parse xml of
        Ok { root } ->
            decode root

        Err _ ->
            withPath [] <| Err InvalidXml


decode : Node -> Result ErrorWithPath (List Post)
decode node =
    node
        |> expectElement "rss"
        |> Result.andThen
            (\rss ->
                rss
                    |> children
                    |> Result.Extra.combineMap (expectElement "channel")
            )
        |> Result.map
            (\channels ->
                channels
                    |> List.concatMap children
                    |> List.filterMap
                        (\child ->
                            child
                                |> expectElement "item"
                                |> Result.toMaybe
                        )
            )
        |> withPath []
        |> Result.andThen (Result.Extra.combineMap parseItem)
        |> Result.map List.reverse


parseItem : Element -> Result ErrorWithPath Post
parseItem element =
    let
        cs : List Node
        cs =
            children element
    in
    cs
        |> foldlResult
            (\e acc ->
                let
                    subpath : List String
                    subpath =
                        [ Maybe.withDefault "?" acc.title ]
                in
                case e of
                    Element eName eAttrs eChildren ->
                        withPath (eName :: subpath) <|
                            case eName of
                                "title" ->
                                    extractContent eChildren
                                        |> Result.map
                                            (\title ->
                                                { acc | title = Just title }
                                            )

                                "link" ->
                                    extractContent eChildren
                                        |> Result.map (\link -> { acc | link = Just link })

                                "description" ->
                                    --     extractContent eChildren
                                    --         |> Result.map (\description -> { acc | description = Just description })
                                    Ok acc

                                "enclosure" ->
                                    case
                                        List.Extra.findMap
                                            (\{ name, value } ->
                                                if name == "url" then
                                                    Just value

                                                else
                                                    Nothing
                                            )
                                            eAttrs
                                    of
                                        Nothing ->
                                            Err <| ExpectedAttributeNotFound { expected = "url", attributes = eAttrs }

                                        Just mediaUrl ->
                                            Ok { acc | mediaUrl = Just mediaUrl }

                                "pubDate" ->
                                    extractContent eChildren
                                        |> Result.andThen parseDate
                                        |> Result.map (\pubDate -> { acc | pubDate = Just pubDate })

                                "itunes:image" ->
                                    case
                                        List.Extra.findMap
                                            (\{ name, value } ->
                                                if name == "href" then
                                                    Just value

                                                else
                                                    Nothing
                                            )
                                            eAttrs
                                    of
                                        Nothing ->
                                            Err <| ExpectedAttributeNotFound { expected = "href", attributes = eAttrs }

                                        Just image ->
                                            Ok { acc | image = Just image }

                                "guid" ->
                                    Ok acc

                                "itunes:episode" ->
                                    Ok acc

                                "itunes:season" ->
                                    Ok acc

                                _ ->
                                    -- Err <| UnexpectedAttribute eName
                                    Ok acc

                    Text _ ->
                        withPath subpath <| Err UnexpectedText
            )
            (Ok
                { title = Nothing
                , link = Nothing
                , image = Nothing

                -- , description = Nothing
                , mediaUrl = Nothing
                , pubDate = Nothing
                }
            )
        |> Result.andThen
            (\acc ->
                let
                    subpath : List String
                    subpath =
                        [ Maybe.withDefault "?" acc.title ]

                    check : a -> String -> Maybe a -> Result Error a
                    check default expected value =
                        case value of
                            Nothing ->
                                Err <| MissingField { expected = expected, children = cs }

                            Just v ->
                                if v == default then
                                    Err <| MissingField { expected = expected, children = cs }

                                else
                                    Ok v

                    checkTime : String -> Maybe Time.Posix -> Result Error Time.Posix
                    checkTime =
                        check (Time.millisToPosix 0)

                    checkString : String -> Maybe String -> Result Error String
                    checkString =
                        check ""
                in
                withPath subpath <|
                    Result.map5
                        (\title image link mediaUrl pubDate ->
                            { title =
                                Parser.run titleParser title
                                    |> Result.withDefault (Other title)
                            , image = image
                            , originalTitle = title
                            , link = link

                            -- , description = description
                            , mediaUrl = mediaUrl
                            , pubDate = pubDate
                            }
                        )
                        (checkString "title" acc.title)
                        (checkString "image" acc.image)
                        (checkString "link" acc.link)
                        -- (checkString "description" acc.description)
                        (checkString "enclosure.url" acc.mediaUrl)
                        (checkTime "pubData" acc.pubDate)
            )


titleParser : Parser Title
titleParser =
    Parser.oneOf
        [ demoParser
        , songIdeaParser
        , voiceMemoParser
        , bonusDemoParser
        , anIdeaADayParser
        , firstDraftFebruaryParser
        , podcastParser
        , audioDiaryParser
        ]
        |. Parser.end


demoParser : Parser Title
demoParser =
    Parser.succeed
        (\( number, name ) ->
            name
                |> String.replace " (+ London show this tuesday)" ""
                |> cleanQuotes
                |> Demo number
        )
        |= Parser.oneOf
            [ Parser.succeed ( Nothing, "lifeline" )
                |. Parser.token "March demo 'lifeline': lyrics & an amended mix!"
            , Parser.succeed ( Nothing, "ragdoll" )
                |. Parser.token "your may demo is not ready (but here is another demo while you wait)"
            , Parser.succeed ( Just 36, "too many feelings (preview)" )
                |. Parser.token "preview: 'too many feelings' (jan demo)"
            , Parser.succeed ( Just 12, "lifeline (version 2)" )
                |. Parser.token "demo no:12 'lifeline'"
            , Parser.succeed ( Nothing, "party by myself" )
                |. Parser.token "a 2016 demo: 'party by myself'"
            , Parser.succeed ( Just 20, "cookie cutter love" )
                |. Parser.token "another live demo: 'cookie cutter lover'"
            , Parser.succeed ( Just 37, "for you" )
                |. Parser.token "demo no (?) - 'for you'"
            , Parser.succeed Tuple.pair
                |. Parser.oneOf
                    [ Parser.token "another live"
                    , Parser.token "old"
                    , Parser.succeed ()
                    ]
                |. Parser.spaces
                |. Parser.token "demo"
                |. Parser.spaces
                |. maybe (Parser.token "no")
                |. Parser.chompWhile (\c -> c == ' ' || c == '.')
                |= Parser.oneOf
                    [ Parser.map Just Parser.float
                    , Parser.map (\_ -> Nothing) <| Parser.token "(?)"
                    , Parser.map (\_ -> Nothing) <| Parser.token "(number i-don't-even-know)"
                    , Parser.succeed Nothing
                    ]
                |. Parser.spaces
                |. maybe (Parser.token "(!)")
                |. maybe (Parser.token "(!!!!!!!)")
                |. Parser.spaces
                |. Parser.oneOf [ Parser.token ":", Parser.token "-" ]
                |. Parser.spaces
                |= chompToEndAndClean
            , Parser.map (Tuple.pair Nothing) <|
                suffixParser " (demo)"
            , Parser.map (Tuple.pair Nothing) <|
                suffixParser " (a lost demo resurrected)"
            ]


voiceMemoParser : Parser Title
voiceMemoParser =
    Parser.succeed (\title -> VoiceMemo (cleanQuotes title))
        |= Parser.oneOf
            [ Parser.succeed "Christmas Eve"
                |. Parser.token "a Christmas Eve voice memo for you"
            , Parser.succeed "an update"
                |. Parser.token "an update via voice memo"
            , Parser.succeed "christmas"
                |. Parser.token "a christmas voice memo"
            , Parser.succeed "overthinking / code red"
                |. Parser.token "'overthinking' / 'code red' voice memo :)"
            , Parser.succeed "A Tuesday voice memo"
                |. Parser.token "a tuesday voice memo"
            , Parser.succeed identity
                |. Parser.oneOf
                    [ Parser.token "new voice memo"
                    , Parser.token "hello! a voice memo"
                    , Parser.token "voice memo"
                    ]
                |. Parser.spaces
                |. maybe (Parser.token "update")
                |. maybe (Parser.token ": ")
                |= chompToEndAndClean
            , suffixParser " (voice memo)"
            , suffixParser " - a 2am voice memo"
            , suffixParser " - voice memo"
            ]


bonusDemoParser : Parser Title
bonusDemoParser =
    Parser.succeed (\title -> BonusDemo (cleanQuotes title))
        |= Parser.oneOf
            [ Parser.succeed "I don't care"
                |. Parser.token "your October bonus demo: 'I don't care'"
            , Parser.succeed "Madison"
                |. Parser.token "gold tier post - Madison "
            , Parser.succeed "God is a woman (cover w/ dodie & Julia Nunes)"
                |. Parser.token "bonus demo: 'god is a woman' (cover w/ dodie & Julia Nunes) "
            , Parser.succeed "Untitled (Some kind of magic?)"
                |. Parser.token "bonus demo for gold tier patrons : )"
            , Parser.succeed identity
                |. Parser.oneOf
                    [ Parser.token "top tier "
                    , Parser.token "gold tier "
                    , Parser.token "february "
                    , Parser.succeed ()
                    ]
                |. Parser.token "bonus"
                |. Parser.spaces
                |. Parser.oneOf
                    [ Parser.token "(instrumental)"
                    , Parser.succeed ()
                    ]
                |. Parser.spaces
                |. Parser.token "demo"
                |. Parser.oneOf [ Parser.token ":", Parser.token "!" ]
                |. Parser.spaces
                |= Parser.getChompedString (Parser.chompUntilEndOr " (")
                |. Parser.chompUntilEndOr ")"
                |. maybe (Parser.token ")")
                |. Parser.spaces
            , suffixParser " (bonus demo)"
            , suffixParser " (bonus demo for you extra kind folk!)"
            ]


songIdeaParser : Parser Title
songIdeaParser =
    Parser.succeed (\title -> SongIdea (cleanQuotes title))
        |= Parser.oneOf
            [ Parser.succeed "Salt"
                |. Parser.token "voice memo: song idea + a THANK U!"
            , Parser.succeed "Change"
                |. Parser.token "voice memo idea 'change' + an apology/request"
            , Parser.succeed identity
                |. maybe (Parser.token "late night ")
                |. maybe (Parser.token "sleepy ")
                |. Parser.oneOf
                    [ Parser.succeed identity
                        |. Parser.oneOf
                            [ Parser.token "an idea returns: "
                            , Parser.token "idea in progress - "
                            , Parser.token "voice memo song idea:"
                            , Parser.token "voice memo idea:"
                            ]
                    , Parser.succeed identity
                        |. Parser.oneOf
                            [ Parser.token "song"
                            , Parser.token "Song"
                            ]
                        |. Parser.oneOf
                            [ Parser.token " idea: "
                            , Parser.token " idea - "
                            , Parser.token " idea "
                            ]
                    ]
                |= Parser.oneOf
                    [ suffixParser "+ an apology/request"
                    , chompToEndAndClean
                    ]
            , suffixParser " (idea)"
            , suffixParser " (song idea)"
            , suffixParser " (song idea!)"
            , suffixParser " (voice memo idea)"
            , suffixParser " (a thursday night song idea)"
            ]


podcastParser : Parser Title
podcastParser =
    Parser.succeed Podcast
        |= Parser.oneOf
            [ Parser.succeed Nothing
                |. Parser.token "podcast pilot"
            , Parser.succeed Just
                |. maybe
                    (Parser.succeed ()
                        |. Parser.oneOf [ Parser.token "podcast", Parser.token "PODCAST" ]
                        |. Parser.token ": "
                    )
                |. maybe (Parser.token "'")
                |. Parser.token "orlaboutanything"
                |. maybe (Parser.token "'")
                |. Parser.spaces
                |. maybe (Parser.token "podcast")
                |. Parser.spaces
                |. Parser.token "episode"
                |. Parser.spaces
                |= Parser.int
                |. maybe (Parser.token "!")
            , Parser.succeed identity
                |. Parser.oneOf
                    [ Parser.token "NEW PODCAST EPISODE"
                    , Parser.token "new podcast episode"
                    ]
                |. Parser.spaces
                |= Parser.oneOf
                    [ Parser.succeed Just
                        |. Parser.token "! episode"
                        |. Parser.spaces
                        |= Parser.int
                    , Parser.succeed Nothing
                    ]
            ]
        |. Parser.spaces
        |. Parser.oneOf [ Parser.token "-", Parser.token ":", Parser.end ]
        |. Parser.spaces
        |= chompToEndAndClean


audioDiaryParser : Parser Title
audioDiaryParser =
    let
        months : List ( String, String )
        months =
            [ ( "January", "Jan" )
            , ( "February", "Feb" )
            , ( "March", "Mar" )
            , ( "April", "Apr" )
            , ( "May", "May" )
            , ( "June", "Jun" )
            , ( "July", "Jul" )
            , ( "August", "Aug" )
            , ( "September", "Sep" )
            , ( "October", "Oct" )
            , ( "November", "Nov" )
            , ( "December", "Dec" )
            ]

        dateParser : Parser (Maybe String)
        dateParser =
            Parser.succeed (\month day -> Just <| month ++ " " ++ String.fromInt day)
                |= Parser.oneOf
                    (months
                        |> List.concatMap
                            (\( long, short ) ->
                                [ Parser.succeed short |. Parser.keyword long
                                , Parser.succeed short |. Parser.keyword (String.toLower long)
                                , Parser.succeed short |. Parser.keyword short
                                , Parser.succeed short |. Parser.keyword (String.toLower short)
                                ]
                            )
                    )
                |. Parser.spaces
                |= Parser.int
                |. Parser.oneOf
                    [ Parser.symbol "th"
                    , Parser.symbol "st"
                    , Parser.symbol "nd"
                    , Parser.symbol "rd"
                    , Parser.succeed ()
                    ]
                |. Parser.spaces
                |. Parser.oneOf
                    [ Parser.symbol ":"
                    , Parser.symbol "-"
                    , Parser.symbol "."
                    , Parser.symbol ","
                    , Parser.symbol "/"
                    , Parser.succeed ()
                    ]
                |. Parser.spaces
                |. Parser.oneOf
                    [ Parser.int
                    , Parser.succeed -1
                    ]
                |. Parser.spaces

        cleanParens : String -> String
        cleanParens s =
            s
                |> cleanOpenParens
                |> cleanCloseParens

        cleanOpenParens : String -> String
        cleanOpenParens s =
            if String.startsWith "(" s then
                String.dropLeft 1 s

            else
                s

        cleanCloseParens : String -> String
        cleanCloseParens s =
            if String.endsWith ")" s then
                String.dropRight 1 s

            else
                s
    in
    Parser.succeed AudioDiary
        |. Parser.oneOf
            [ Parser.keyword "new" |. Parser.spaces
            , Parser.succeed ()
            ]
        |. Parser.token "audio diary"
        |. Parser.spaces
        |. Parser.oneOf
            [ Parser.token ":"
            , Parser.token ","
            , Parser.token "/"
            ]
        |. Parser.spaces
        |= Parser.oneOf
            [ dateParser
            , Parser.succeed Nothing
            ]
        |. Parser.spaces
        |= Parser.map cleanParens chompToEndAndClean


anIdeaADayParser : Parser Title
anIdeaADayParser =
    Parser.succeed
        (\( when, what ) ->
            let
                known : List ( Result String number, String )
                known =
                    [ ( Ok 1, "Did it to myself" )
                    , ( Ok 2, "First date" )
                    , ( Ok 3, "oh GOD" )
                    , ( Ok 4, "Pity party" )
                    , ( Ok 5, "Drowning" )
                    , ( Ok 6, "What now" )
                    , ( Ok 7, "It's impressive right" )
                    , ( Ok 8, "Pretending" )
                    , ( Ok 9, "Change" )
                    , ( Ok 10, "Long looong summer" )
                    , ( Ok 11, "Heavy" )
                    , ( Ok 12, "Back to sleep" )
                    , ( Ok 13, "Somehow" )
                    , ( Ok 15, "One more strike" )
                    , ( Ok 30, "Hotel breakfast" )
                    , ( Err "may 31st", "Dumb" )
                    , ( Err "june 5th", "Don't fall in love with a musician" )
                    , ( Err "june 12th", "Because of you" )
                    ]
            in
            case List.Extra.find (\( day, _ ) -> day == when) known of
                Just ( _, name ) ->
                    AnIdeaADay when name

                Nothing ->
                    AnIdeaADay when what
        )
        |. Parser.token "an idea "
        |. Parser.oneOf [ Parser.token "a day", Parser.token "(some) days" ]
        |. Parser.token " (for the month of may)"
        |= Parser.oneOf
            [ Parser.succeed (\m -> ( Err m, "" ))
                |. Parser.token " (+ june lol) "
                |= chompToEndAndClean
            , Parser.backtrackable <|
                Parser.succeed (\m -> ( Ok m, "" ))
                    |. Parser.spaces
                    |. Parser.token ": "
                    |. Parser.oneOf
                        [ Parser.token "no"
                        , Parser.token "day "
                        , Parser.token "day"
                        ]
                    |= Parser.int
                    |. maybe (Parser.token " (oops)")
            , Parser.succeed (\t m -> ( Err m, t ))
                |. Parser.token ": '"
                |= Parser.getChompedString (Parser.chompUntil "'")
                |. Parser.token "' ("
                |= Parser.getChompedString (Parser.chompUntil ")")
                |. Parser.token ")"
            ]


firstDraftFebruaryParser : Parser Title
firstDraftFebruaryParser =
    Parser.succeed (\when what -> FirstDraftFebruary when what)
        |. Parser.token "first draft february"
        |. Parser.spaces
        |. maybe (Parser.oneOf [ Parser.token "/", Parser.token ":" ])
        |. Parser.spaces
        |. maybe (Parser.token "no")
        |. Parser.spaces
        |. maybe (Parser.token ".")
        |. Parser.spaces
        |= Parser.map String.toInt (Parser.getChompedString <| Parser.chompWhile Char.isDigit)
        |. Parser.spaces
        |. Parser.oneOf [ Parser.token ".", Parser.token ":" ]
        |. Parser.spaces
        |= chompToEndAndClean


chompToEndAndClean : Parser String
chompToEndAndClean =
    Parser.map cleanQuotes <|
        Parser.getChompedString <|
            Parser.chompWhile (\_ -> True)


maybe : Parser () -> Parser ()
maybe parser =
    Parser.oneOf
        [ parser
        , Parser.succeed ()
        ]


{-| Parse "anything followed by a specific suffix". The parsed string will not include the suffix.
-}
suffixParser : String -> Parser String
suffixParser suffix =
    Parser.backtrackable <|
        Parser.succeed identity
            |= Parser.getChompedString (Parser.chompUntil suffix)
            |. Parser.token suffix


cleanQuotes : String -> String
cleanQuotes s =
    let
        trimmed : String
        trimmed =
            s
                |> String.replace ": )" ""
                |> String.replace ":)" ""
                |> String.trim
    in
    if String.startsWith "'" trimmed && String.endsWith "'" trimmed then
        String.slice 1 -1 trimmed

    else
        trimmed


parseDate : String -> Result Error Time.Posix
parseDate date =
    date
        |> Imf.DateTime.toPosix
        |> Result.mapError (\_ -> InvalidDate date)


extractContent : List Node -> Result Error String
extractContent cs =
    case cs of
        [] ->
            Ok ""

        [ Text c ] ->
            Ok c

        _ ->
            Err ExpectedTextGotNodes


foldlResult : (elem -> acc -> Result err acc) -> Result err acc -> List elem -> Result err acc
foldlResult step =
    List.foldl (\e -> Result.andThen (step e))


expectElement : String -> Node -> Result Error Element
expectElement expectedName node =
    case node of
        Text text ->
            Err <| ExpectedNodeGotText { expected = expectedName, text = text }

        Element actualName actualAttributes actualChildren ->
            if actualName == expectedName then
                Ok ( expectedName, actualAttributes, actualChildren )

            else
                Err <| ExpectedNodeGotOtherNode { expected = expectedName, actual = actualName }


withPath : Path -> Result Error x -> Result ErrorWithPath x
withPath path =
    Result.mapError
        (\error ->
            { error = error
            , path = List.reverse path
            }
        )


type alias Element =
    ( String, List Attribute, List Node )


children : Element -> List Node
children ( _, _, nodes ) =
    List.filterMap cleanNode nodes


cleanNode : Node -> Maybe Node
cleanNode node =
    case node of
        Text content ->
            let
                clean : String
                clean =
                    String.trim content
            in
            if String.isEmpty clean then
                Nothing

            else
                Just node

        Element _ _ _ ->
            Just node


type alias ErrorWithPath =
    { error : Error
    , path : Path
    }


type alias Path =
    List String


type Error
    = InvalidXml
    | ExpectedNodeGotText { expected : String, text : String }
    | ExpectedNodeGotOtherNode { expected : String, actual : String }
    | MissingField { expected : String, children : List Node }
    | UnexpectedAttribute String
    | UnexpectedText
    | ExpectedTextGotNodes
    | ExpectedAttributeNotFound { expected : String, attributes : List Attribute }
    | InvalidDate String


rssPrefix : String
rssPrefix =
    "https://www.patreon.com/rss/orlagartland?auth="


errorToString : Error -> String
errorToString err =
    case err of
        InvalidXml ->
            "Invalid XML"

        ExpectedNodeGotText _ ->
            "ExpectedNodeGotText _"

        ExpectedNodeGotOtherNode _ ->
            "ExpectedNodeGotOtherNode _"

        MissingField _ ->
            "MissingField _"

        UnexpectedAttribute name ->
            "UnexpectedAttribute " ++ name

        UnexpectedText ->
            "UnexpectedText"

        ExpectedTextGotNodes ->
            "ExpectedTextGotNodes"

        ExpectedAttributeNotFound _ ->
            "ExpectedAttributeNotFound _"

        InvalidDate _ ->
            "InvalidDate _"
