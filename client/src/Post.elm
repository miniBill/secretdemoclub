module Post exposing (Post, isMatch)

import Route exposing (Filter)
import Set
import Time
import Url exposing (Url)


type alias Post =
    { title : String
    , category : String
    , date : Time.Posix
    , image : String
    , link : Url
    , media : String
    , number : Maybe String
    }


isMatch :
    Filter
    -> Time.Zone
    -> { title : String, image : String, link : Url, media : String, number : Maybe String, date : Time.Posix, category : String }
    -> Bool
isMatch filter here post =
    isCorrectCategory filter post
        && isCorrectYear filter here post
        && isTextMatch filter.search post


isCorrectCategory : Filter -> { title : String, image : String, link : Url, media : String, number : Maybe String, date : Time.Posix, category : String } -> Bool
isCorrectCategory filter post =
    Set.isEmpty filter.categories
        || Set.member "all" filter.categories
        || Set.member (String.toLower post.category) filter.categories
        || Set.member (String.toLower post.category ++ "s") filter.categories


isCorrectYear : Filter -> Time.Zone -> { title : String, image : String, link : Url, media : String, number : Maybe String, date : Time.Posix, category : String } -> Bool
isCorrectYear filter here post =
    case filter.year of
        Nothing ->
            True

        Just year ->
            Time.toYear here post.date == year


isTextMatch : String -> Post -> Bool
isTextMatch needle post =
    let
        normalize : String -> String
        normalize input =
            input
                |> String.trim
                |> String.toLower

        checkPiece : String -> Bool
        checkPiece piece =
            let
                cleanPiece : String
                cleanPiece =
                    normalize piece
            in
            String.isEmpty cleanPiece
                || String.contains cleanPiece (normalize post.title)
                || String.contains cleanPiece (normalize post.category)
                || String.contains cleanPiece (normalize (Maybe.withDefault "" post.number))
    in
    needle
        |> String.split " "
        |> List.all checkPiece
