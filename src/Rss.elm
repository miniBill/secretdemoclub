module Rss exposing (Post, Rss, Title(..), getAlbum, lastCodec, titleToString, v1Codec)

import Serialize as Codec exposing (Codec)
import String.Extra
import Time


type alias Rss =
    List Post


v1Codec : Codec () Rss
v1Codec =
    Codec.list v1PostCodec


lastCodec : Codec () Rss
lastCodec =
    v1Codec


type alias Post =
    { title : Title
    , originalTitle : String
    , link : String
    , mediaUrl : String
    , pubDate : Time.Posix
    }


v1PostCodec : Codec () Post
v1PostCodec =
    let
        postPrefix : String
        postPrefix =
            "https://www.patreon.com/posts/"
    in
    Codec.record
        (\title originalTitle link mediaUrl pubDate ->
            { title = title
            , originalTitle = originalTitle
            , link = postPrefix ++ link
            , mediaUrl = mediaUrl
            , pubDate = pubDate
            }
        )
        |> Codec.field .title titleCodec
        |> Codec.field .originalTitle Codec.string
        |> Codec.field (\{ link } -> String.dropLeft (String.length postPrefix) link) Codec.string
        |> Codec.field .mediaUrl Codec.string
        |> Codec.field .pubDate timeCodec
        |> Codec.finishRecord


timeCodec : Codec () Time.Posix
timeCodec =
    Codec.map Time.millisToPosix Time.posixToMillis Codec.int


type Title
    = Demo (Maybe Float) String
    | VoiceMemo String
    | BonusDemo String
    | SongIdea String
    | Podcast (Maybe Int) String
    | AnIdeaADay (Result String Int) String
    | FirstDraftFebruary (Maybe Int) String
    | AudioDiary (Maybe String) String
    | Other String


titleCodec : Codec () Title
titleCodec =
    Codec.customType
        (\fDemo fVoiceMemo fBonusDemo fSongIdea fPodcast fAnIdeaADay fFirstDraftFebruary fAudioDiary fOther value ->
            case value of
                Demo mf s ->
                    fDemo mf s

                VoiceMemo s ->
                    fVoiceMemo s

                BonusDemo s ->
                    fBonusDemo s

                SongIdea s ->
                    fSongIdea s

                Podcast mi s ->
                    fPodcast mi s

                AnIdeaADay r s ->
                    fAnIdeaADay r s

                FirstDraftFebruary mi s ->
                    fFirstDraftFebruary mi s

                AudioDiary ms s ->
                    fAudioDiary ms s

                Other s ->
                    fOther s
        )
        |> Codec.variant2 Demo (Codec.maybe Codec.float) Codec.string
        |> Codec.variant1 VoiceMemo Codec.string
        |> Codec.variant1 BonusDemo Codec.string
        |> Codec.variant1 SongIdea Codec.string
        |> Codec.variant2 Podcast (Codec.maybe Codec.int) Codec.string
        |> Codec.variant2 AnIdeaADay (Codec.result Codec.string Codec.int) Codec.string
        |> Codec.variant2 FirstDraftFebruary (Codec.maybe Codec.int) Codec.string
        |> Codec.variant2 AudioDiary (Codec.maybe Codec.string) Codec.string
        |> Codec.variant1 Other Codec.string
        |> Codec.finishCustomType


getAlbum : Post -> String
getAlbum { title } =
    case title of
        Demo _ _ ->
            "Demos"

        BonusDemo _ ->
            "Bonus demos"

        AnIdeaADay _ _ ->
            "An idea a day"

        FirstDraftFebruary _ _ ->
            "First draft february"

        SongIdea _ ->
            "Song ideas"

        VoiceMemo _ ->
            "Voice memos"

        Podcast _ _ ->
            "Podcast"

        AudioDiary _ _ ->
            "Audio diaries"

        Other _ ->
            "Others"


titleToString : Title -> String
titleToString title =
    case title of
        Demo _ demoTitle ->
            firstUpperWithQuotes demoTitle

        VoiceMemo memoTitle ->
            firstUpperWithQuotes memoTitle

        BonusDemo bonusDemoTitle ->
            firstUpperWithQuotes bonusDemoTitle

        SongIdea songIdeaTitle ->
            firstUpperWithQuotes songIdeaTitle

        Podcast _ podcastTitle ->
            firstUpperWithQuotes podcastTitle

        AnIdeaADay number ideaTitle ->
            if String.isEmpty ideaTitle then
                case number of
                    Ok n ->
                        "An idea a day - Day " ++ String.fromInt n

                    Err day ->
                        "An idea a day - " ++ firstUpperWithQuotes day

            else
                firstUpperWithQuotes ideaTitle

        FirstDraftFebruary _ draftTitle ->
            firstUpperWithQuotes draftTitle

        AudioDiary date diaryTitle ->
            if String.isEmpty diaryTitle then
                "Audio diary - " ++ Maybe.withDefault "" date

            else
                firstUpperWithQuotes diaryTitle

        Other otherTitle ->
            firstUpperWithQuotes otherTitle


firstUpperWithQuotes : String -> String
firstUpperWithQuotes title =
    if String.startsWith "\"" title || String.startsWith "'" title then
        String.left 1 title ++ String.Extra.toSentenceCase (String.dropLeft 1 title)

    else
        String.Extra.toSentenceCase title
