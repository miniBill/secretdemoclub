module Rss exposing (Post, Title(..), getAlbum, titleToString)

import String.Extra
import Time


type alias Post =
    { title : Title
    , image : String
    , originalTitle : String
    , link : String
    , mediaUrl : String
    , pubDate : Time.Posix
    }


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
    | WritingSomethingEveryDay Int String


getAlbum : Title -> String
getAlbum title =
    case title of
        Demo _ _ ->
            "Demos"

        BonusDemo _ ->
            "Bonus demos"

        AnIdeaADay _ _ ->
            "An idea a day"

        WritingSomethingEveryDay _ _ ->
            "Writing something every day"

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

        AnIdeaADay _ ideaTitle ->
            firstUpperWithQuotes ideaTitle

        WritingSomethingEveryDay _ somethingTitle ->
            firstUpperWithQuotes somethingTitle

        FirstDraftFebruary _ draftTitle ->
            firstUpperWithQuotes draftTitle

        AudioDiary _ diaryTitle ->
            firstUpperWithQuotes diaryTitle

        Other otherTitle ->
            firstUpperWithQuotes otherTitle


firstUpperWithQuotes : String -> String
firstUpperWithQuotes title =
    if String.startsWith "\"" title || String.startsWith "'" title then
        String.left 1 title ++ String.Extra.toSentenceCase (String.dropLeft 1 title)

    else
        String.Extra.toSentenceCase title
