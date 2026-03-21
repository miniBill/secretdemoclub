module Rss exposing (Post, Title(..), getAlbum, titleToString)

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
            demoTitle

        VoiceMemo memoTitle ->
            memoTitle

        BonusDemo bonusDemoTitle ->
            bonusDemoTitle

        SongIdea songIdeaTitle ->
            songIdeaTitle

        Podcast _ podcastTitle ->
            podcastTitle

        AnIdeaADay _ ideaTitle ->
            ideaTitle

        WritingSomethingEveryDay _ somethingTitle ->
            somethingTitle

        FirstDraftFebruary _ draftTitle ->
            draftTitle

        AudioDiary _ diaryTitle ->
            diaryTitle

        Other otherTitle ->
            otherTitle
